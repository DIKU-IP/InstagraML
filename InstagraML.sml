(* Simple image manipulation library designed and implemented by
 Troels Henriksen (athas@sigkill.dk) and Martin Dybdal
 (dybber@dybber.dk), for the introductionary programming course at
 DIKU.

 The idea is to use high-level combinators to demonstrate simple
functional programming, with a visual result.  This is only suitable
for simple tasks.
 *)

(* Low-level BMP serialisation stuff originally taken from
 http://www.soc.napier.ac.uk/~cs66/course-notes/sml/bmp.htm.  Troels
 Henriksen modified it to support (only) 24-bit bitmaps, with no
 colour map.
 *)

signature IMAGE = sig
    (* A colour is a triplet of red, green and blue values, in that
    order, each going from 0 to 255. *)
    type colour = int * int * int
    type image

    val width : image -> int
    val height : image -> int
    val fromFunction : int * int * (int * int -> colour) -> image
    val toFunction : image -> int * int * (int * int -> colour)
end

structure DelayedImage :> IMAGE = struct
    type colour = int * int * int
    type image = int * int * (int * int -> colour)

    fun width (w,_,_) = w
    fun height (_,h,_) = h
    fun fromFunction (w,h,f) = (w,h,f)
    fun toFunction img = img
end

signature TRANSFORM = sig
    type colour = int * int * int
    type image
    (* New combinators. *)
    val recolour : (colour -> colour) -> image -> image
    val transform : (real*real -> real*real) -> image -> image
    val scale : real -> real -> image -> image
    val clockwise : image -> image
    val beside : image -> image -> image
end

functor TransformFN (I : IMAGE)
        :> TRANSFORM where type image = I.image = struct
type colour = int * int * int
type image = I.image

fun recolour f img =
    let val (w, h, g) = I.toFunction img
    in I.fromFunction (w, h, f o g) end

fun transform f img =
    let val (w,h, g) = I.toFunction img
        fun toSq (x,y) = (2.0*real x/real w - 1.0,
                          2.0*real y/real h - 1.0)
	fun frmSq (x,y) = (floor ((x+1.0)*real w/2.0),
                           floor ((y+1.0)*real h/2.0))
    in I.fromFunction (w, h, g o frmSq o f o toSq)
    end

fun scale sx sy img =
    let val (w,h,f) = I.toFunction img
        val w' = floor (real w * sx)
        val h' = floor (real h * sy)
        fun f' (x,y) =
            f (floor (real x / sx),
               floor (real y / sy))
    in I.fromFunction (w', h', f') end

fun clockwise img =
    let val (w,h,f) = I.toFunction img
        fun rotate (x,y) = (w-y-1,x)
    in I.fromFunction (h, w, f o rotate) end

fun beside img1 img2 =
    let val (w1,h1,f1) = I.toFunction img1
        val (w2,h2,f2) = I.toFunction img2
        fun f (x,y) = if x < w1
                      then f1 (x,y)
                      else f2 (x-w1,y)
    in I.fromFunction (w1+w2, Int.max(h1,h2), f) end
end

(* BMP serialisation functor *)
functor BMPFN (I : IMAGE) =
struct
local
    fun pad4 x = ~4*(~x div 4)
    (* For 0xRRGGBB, returns (0xRR, 0xGG, 0xBB) *)
    fun channels x =
        ((x div 0x10000) mod 0x100,
         (x div 0x100) mod 0x100,
         x mod 0x100)
    fun get(s,i) = Word8.toInt(Word8Vector.sub(s,i))
    fun get2(s,i)=get(s,i)+256*get(s,i+1)
    fun get3(s,i) = get(s,i)+256*get2(s,i+1)
    fun get4(s,i)=get2(s,i)+256*256*get2(s,i+2)
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
            val get = case bitspp of
                          24 => (fn (x,y) => get3(bitMap, dy*y+x*3))
                        | 32 => (fn (x,y) => get4(bitMap, dy*y+x*4))
                        | _  => raise Fail "Can only handle 24-bit or 32-bit BMPs"
            fun col(x,y) = if outrange(x,y) then 0 else
                           get(x,y)
        in (w,h,colTab,col)
        end

    (* width, height, colour table, (x,y)->pixel value, filename *)
    fun writebmp (w,h,c,t) s =
        let val fh = BinIO.openOut s
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
                        (* At end of row, we have to pad to the next word boundary. *)
                        (if (i+1) mod w = 0 then
                             pad (padw-w)
                         else ());
                        write (i+1) end
        in write 0; BinIO.closeOut fh end
in
fun readBMP s =
    let val (w,h,_,t) = readbmp s
        fun f pos =
            channels (t pos)
    in I.fromFunction (w, h, f) end

fun writeBMP (s, img) =
    let val (w,h,f) = I.toFunction img
        fun f' pos =
            let val (r,g,b) = f pos
            in b + g * 0x100 + r * 0x10000 end
        val c = Word8Vector.fromList []
    in writebmp (w, h, c, f') s
    end
end
end

(* Simple non-functor instantiations. *)
structure InstagraML = struct
local
    structure BMP = BMPFN(DelayedImage);
    structure InstagraML = TransformFN(DelayedImage)
in
(* Basic image interaction *)
val width = DelayedImage.width
val height = DelayedImage.height
val fromFunction = DelayedImage.fromFunction
val toFunction = DelayedImage.toFunction

(* Simple manipulations. *)
val recolour = InstagraML.recolour
val transform = InstagraML.transform
val scale = InstagraML.scale
val clockwise = InstagraML.clockwise
val beside = InstagraML.beside

(* BMP reading functions. *)
val readBMP = BMP.readBMP
val writeBMP = BMP.writeBMP
end
end
