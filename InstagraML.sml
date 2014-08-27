(*

 Simple image manipulation library designed and implemented by Troels
 Henriksen (athas@sigkill.dk) and Martin Dybdal (dybber@dybber.dk),
 for the introductionary programming course at DIKU.  

 The idea is to use high-level combinators to demonstrate simple
 functional programming, with a visual result.  This is only suitable
 for simple tasks.
 *)

(*

 Low-level BMP serialisation stuff originally taken from
 http://www.soc.napier.ac.uk/~cs66/course-notes/sml/bmp.htm.  Troels
 Henriksen modified it to support (only) 24-bit bitmaps, with no colour
 map.
 *)

signature INSTAGRAML = sig
    (* A colour is a triplet of red, green and blue values, in that
    order, each going from 0 to 255. *)
    type colour = int * int * int
    type image

    val readBMP : string -> image
    val writeBMP : string * image -> unit
    val width : image -> int
    val height : image -> int
    val fromFunction : int * int -> (int * int -> colour) -> image
    val pixel : (int * int) -> image -> colour

    val recolour : (colour -> colour) -> image -> image
    val transform : (real*real -> real*real) -> image -> image
    val scale : real -> real -> image -> image
    val clockwise : image -> image
    val beside : image -> image -> image
end

structure InstagraML :> INSTAGRAML = struct
fun pad4 x = ~4*(~x div 4)
(* For 0xRRGGBB, returns (0xRR, 0xGG, 0xBB) *)
fun channels x =
    ((x div 0x10000) mod 0x100,
     (x div 0x100) mod 0x100,
     x mod 0x100)

local
    fun get(s,i) = Word8.toInt(Word8Vector.sub(s,i))
    fun get2(s,i)=get(s,i)+256*get(s,i+1)
    fun get3(s,i) = get(s,i)+256*get2(s,i+1)
    fun get4(s,i)=get2(s,i)+256*256*get2(s,i+2)
in
fun readbmp s =
    let
        val input = BinIO.inputN
        val fh = BinIO.openIn s
        val header = input(fh,54)
        val bitspp = get2(header,0x1C)
        val colTab = input(fh,get4(header,0xA) - 54)
        val bitMap = input(fh,get4(header,0x22))
        val w = get4(header,0x12)
        val h = get4(header,0x16)
        val dy = pad4((bitspp * w) div 8)
        fun outrange(x,y)=x<0 orelse y<0 orelse x>=w orelse y>=h
        fun col(x,y) = if outrange(x,y) then 0 else
                       get3(bitMap,dy*y+x*3)
    in if bitspp = 24
       then (w,h,colTab,col)
       else raise Domain
    end
end
local
    fun mk2 v = Word8Vector.fromList[Word8.fromInt(v mod 256),
                                     Word8.fromInt(v div 256 mod 256)]
    fun mk4 v = Word8Vector.concat[mk2 (v mod 65536),mk2 (v div 65536)]
    fun upto n m = if n=m then [n] else n::(upto (n+1) m)
    fun header w h c = let
        val size = Word8Vector.length in
        Word8Vector.concat[
        Word8Vector.fromList(map(Word8.fromInt o ord) [#"B",#"M"]),
        mk4((pad4 w)*h*3+size c+54),
        mk4 0, mk4 (size c+54), mk4 (40+size c),
        mk4 w,  mk4 h,  mk2 1,  mk2 24, mk4 0,  mk4 ((pad4 w)*3*h),
        mk4 2834, mk4 2834, mk4 0, mk4 0] end
in
(* width, height, colour table, (x,y)->pixel value, filename *)
fun writebmp (w,h,c,t) s = let
    val fh = BinIO.openOut s
    val () = BinIO.output(fh,header w h c)
    val () = BinIO.output(fh,c)
    val padw = pad4 w
    fun put1 x = BinIO.output1(fh, Word8.fromInt x)
    fun pad 0 = ()
      | pad n = put1 0 before pad (n-1)
    fun write i =
        if i = padw*h then ()
        else let val (r,g,b) = channels(t(i mod w, i div w))
             in put1 b; put1 g; put1 r;
                (* At end of row, we have to pad to the next word
                                     boundary. *)
                (if (i+1) mod w = 0 then
                     pad (padw-w)
                 else ());
                write (i+1) end
in write 0; BinIO.closeOut fh end end

type colour = int * int * int
type image = int * int * ((int*int) -> colour)

fun width (w, _, _) = w

fun height (_, h, _) = h

fun readBMP s =
    let val (w,h,_,t) = readbmp s
        fun f pos =
            channels (t pos)
    in (w,h, f) end

fun writeBMP (s, (w, h, f)) =
    let fun f' pos =
            let val (r,g,b) = f pos
            in b + g * 0x100 + r * 0x10000 end
        val c = Word8Vector.fromList []
    in writebmp (w,h,c,f') s end

fun fromFunction (w, h) f = (w, h, f)

fun pixel pos (_, _, f) = f pos

fun recolour g (w,h,f) = (w,h, g o f)

fun transform g (w,h,f) =
    let fun toSq (x,y) = (2.0*real x/real w - 1.0,
                          2.0*real y/real h - 1.0)
	fun frmSq(x,y)=(floor ((x+1.0)*real w/2.0),
                        floor ((y+1.0)*real h/2.0))
    in (w, h, f o frmSq o g o toSq)
    end

fun scale sx sy (w,h,f) =
    let val w' = floor (real w * sx)
        val h' = floor (real h * sy)
        fun f' (x,y) =
            f (floor (real x / sx),
                   floor (real y / sy))
    in (w', h', f') end

fun clockwise (w,h,f) =
    let fun swap (x,y) = (w-y-1,x)
    in (h, w, f o swap) end

fun beside (w1,h1,f1) (w2,h2,f2) =
    let fun f (x,y) = if x < w1
                          then f1 (x,y)
                          else f2 (x-w1,y)
    in (w1+w2, Int.max(h1,h2), f) end
end

val counterClockwise = InstagraML.clockwise o InstagraML.clockwise o InstagraML.clockwise

fun below x y =
    counterClockwise (InstagraML.beside (InstagraML.clockwise x) (InstagraML.clockwise y))

fun four a b c d = InstagraML.beside (below a b) (below c d)
