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
    val fromFunction : int * int -> (int * int -> colour) -> image
    val pixel : image -> (int * int) -> colour
end

structure DelayedImage :> IMAGE = struct
    type colour = int * int * int
    type image = int * int * (int * int -> colour)

    fun width (w,_,_) = w
    fun height (_,h,_) = h
    fun fromFunction (w,h) f = (w,h,f)
    fun pixel (_,_,f) = f
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
    I.fromFunction (I.width img, I.height img) (f o I.pixel img)

fun transform f img =
    let val w = I.width img
        val h = I.height img
        fun toSq (x,y) = (2.0*real x/real w - 1.0,
                          2.0*real y/real h - 1.0)
	fun frmSq (x,y) = (floor ((x+1.0)*real w/2.0),
                           floor ((y+1.0)*real h/2.0))
    in I.fromFunction (w, h) (I.pixel img o frmSq o f o toSq)
    end

fun scale sx sy img =
    let val w = I.width img
        val h = I.height img
        val w' = floor (real w * sx)
        val h' = floor (real h * sy)
        fun f' (x,y) =
            I.pixel img (floor (real x / sx),
                         floor (real y / sy))
    in I.fromFunction (w', h') f' end

fun clockwise img =
    let fun rotate (x,y) = (I.width img-y-1,x)
    in I.fromFunction (I.height img, I.width img) (I.pixel img o rotate) end

fun beside img1 img2 =
    let val w1 = I.width img1
        val h1 = I.height img1
        val w2 = I.width img2
        val h2 = I.height img2
        fun f (x,y) = if x < w1
                      then I.pixel img1 (x,y)
                      else I.pixel img2 (x-w1,y)
    in I.fromFunction (w1+w2, Int.max(h1,h2)) f end
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
            fun col(x,y) = if outrange(x,y) then 0 else
                           get3(bitMap,dy*y+x*3)
        in if bitspp = 24
           then (w,h,colTab,col)
           else raise Domain
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
    in I.fromFunction (w,h) f end

fun writeBMP (s, img) =
    let fun f' pos =
            let val (r,g,b) = I.pixel img pos
            in b + g * 0x100 + r * 0x10000 end
        val c = Word8Vector.fromList []
    in writebmp (I.width img,
                 I.height img,
                 c,
                 f') s
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
val pixel = DelayedImage.pixel

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
