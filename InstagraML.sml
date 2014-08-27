(*

 Simple image manipulation library designed and implemented by Troels
 Henriksen (athas@sigkill.dk) and Martin Dybdal (dybber@dybber.dk),
 possibly for the introductionary programmint course at DIKU.  The
 idea is to use high-level combinators to demonstrate simple
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
        fun pixel pos =
            channels (t pos)
    in (w,h, pixel) end

fun writeBMP (s, (w, h, pixel)) =
    let fun pixel' pos =
            let val (r,g,b) = pixel pos
            in b + g * 0x100 + r * 0x10000 end
        val c = Word8Vector.fromList []
    in writebmp (w,h,c,pixel') s end

fun recolour f (w,h,pixel) = (w,h, f o pixel)

fun transform f (w,h,pixel) =
    let fun toSq (x,y) = (2.0*real x/real w - 1.0,
                          2.0*real y/real h - 1.0)
	fun frmSq(x,y)=(floor ((x+1.0)*real w/2.0),
                        floor ((y+1.0)*real h/2.0))
    in (w, h, pixel o frmSq o f o toSq)
    end

fun scale sx sy (w,h,pixel) =
    let val w' = floor (real w * sx)
        val h' = floor (real h * sy)
        fun pixel' (x,y) =
            pixel (floor (real x / sx),
                   floor (real y / sy))
    in (w', h', pixel') end

fun clockwise (w,h,pixel) =
    let fun swap (x,y) = (w-y-1,x)
    in (h, w, pixel o swap) end

fun beside (w1,h1,pixel1) (w2,h2,pixel2) =
    let fun pixel (x,y) = if x < w1
                          then pixel1 (x,y)
                          else pixel2 (x-w1,y)
    in (w1+w2, Int.max(h1,h2), pixel) end
end

(* Example transformations (for InstagraML.transform), from
http://www.soc.napier.ac.uk/~cs66/course-notes/sml/bmp.htm *)
fun relf(x,y)=(~x:real,y)
fun blow(x,y) = (x*0.5,y*0.5)
fun fish(x,y)=let val r=Math.sqrt(x*x+y*y) in (r*x,r*y) end
fun unfish p (x,y)=let val r=(Math.sqrt(x*x+y*y)+p)/(1.0+p) in (x/r,y/r) end
fun wasp(x,y)=(x/(y*y+1.0)*2.0,y)
fun fat p (x,y) = (x*(y*y+p)/p,y:real)
fun rot a (x,y) =let val c=Math.cos a val s=Math.sin a in(x*c-y*s,x*s+y*c)end
fun whirl(x,y)=let val r=Math.sqrt(x*x+y*y) in rot (1.0-r)(x,y) end
fun wave(x,y)=(x+Math.sin(3.0*y)/4.0,y)
fun shear(x,y)=(x+y/2.0,y)
fun polo(x,y)=(2.0*Math.atan(x/y)/3.1415,Math.sqrt(x*x+y*y)-0.2)

(* Example operations. *)
val invertColours = InstagraML.recolour (fn (r,g,b) => (255-r, 255-g, 255-b))

val desaturate = InstagraML.recolour (fn (r,g,b) =>
                                    let val x = (r+r+g+g+g+b) div 6
                                    in (x, x, x) end)

val counterClockwise = InstagraML.clockwise o InstagraML.clockwise o InstagraML.clockwise

fun below x y =
    counterClockwise (InstagraML.beside (InstagraML.clockwise x) (InstagraML.clockwise y))

fun four a b c d = InstagraML.beside (below a b) (below c d)

(* Warhol effect *)
fun warholEffect image colours =
  let val n = length colours;
      val range = 256 div n;
      fun intensity (r,g,b) = (r+g+b) div 3
      fun selectColour x = List.nth(colours, Int.min(x div range, n-1));
  in
      InstagraML.recolour (selectColour o intensity) image
  end;

val black   = (0,0,0)
val white   = (255,255,255)
val red     = (0,0,255)
val green   = (0,255,0)
val blue    = (255,0,0)
val yellow  = (0,255,255)
val cyan    = (255,255,0)
val magenta = (255,0,255)
val orange  = (0,165,255)
val pink    = (203,192,255)
val purple  = (128,0,128)

val colours1 = [purple, pink, pink, cyan]
val colours2 = [blue, magenta, orange, yellow]
val colours3 = [red, red, yellow, green]
val colours4 = [black, red, red, white]

fun warhol img = four (warholEffect img colours1)
                      (warholEffect img colours2)
                      (warholEffect img colours3)
                      (warholEffect img colours4);

fun quad img = four img img img img

fun spiral img 0 = img
  | spiral img n =
    let val a = InstagraML.scale 0.5 0.5 (quad img)
        val b = InstagraML.scale 0.5 0.5 (quad a)
    in InstagraML.beside
           (below img a)
           (InstagraML.clockwise (InstagraML.beside b (InstagraML.clockwise (spiral (InstagraML.scale 0.5 0.5 a) (n-1)))))
    end

(* Try: InstagraML.writeBMP ("spiral.bmp", spiral (InstagraML.readBMP "torben.bmp") 10); *)

local
  datatype complex = C of real * real
  infix 7 **
  infix 6 ++

  fun (C (a, b)) ** (C (c, d)) = C (a*c-b*d, a*d+b*c)
  fun (C (a, b)) ++ (C (c, d)) = C (a+c, b+d)
  fun abs (C (a, b)) = Math.sqrt (a*a + b*b)

  fun color n = let val v = 255 - 255 - Int.min (n * 5, 255) in (v, v, v) end
  fun divergence z c 60 = 0
    | divergence z c i  = let val z' = z ** z ++ c in
                            if abs z' > 4.0
                            then i
                            else divergence z' c (i+1)
                          end
  fun mandelbrot_ (x,y) =
    let val a = 3.5 / (x+0.01) - 2.5
        val b = 3.0 / (y+0.01) - 1.5
        val c = real (divergence (C (0.0, 0.0)) (C (a, b)) 0) / 50.0
    in (x*c,y*c) end
in
  fun mandelbrot image = InstagraML.transform mandelbrot_ image
end;
