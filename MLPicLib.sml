(* BMP reading/writing stuff originally taken from:

 http://www.soc.napier.ac.uk/~cs66/course-notes/sml/bmp.htm
*)

(*
 Troels Henriksen (athas@sigkill.dk) modified it to support (only)
 24-bit bitmaps, with no color map.
 *)

signature PICLIB = sig
    type color = int * int * int
    type image

    val readBMP : string -> image
    val writeBMP : string * image -> unit

(*
 val recolor : (color -> color) -> image -> image
 val transform : (real*real -> real*real) -> image -> image
 val scale : real -> image -> image
 val beside : image -> image -> image
 val clockwise : image -> image
 val torben : image (* https://www2.adm.ku.dk/selv/pls/prt_www40.hentindhold_cms?p_personid=162114 *)
 *)
end

structure PicLib : PICLIB = struct
type color = int * int * int
type image = int * int * ((int*int) -> color)

fun pad4 x = ~4*(~x div 4)
fun channels x =
    (x mod 0x100,
     (x div 0x100) mod 0x100,
     (x div 0x10000) mod 0x100)

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
    fun f i =
        let val (r,g,b) = channels(t(i mod padw, i div padw))
        in map Word8.fromInt [r,g,b] end
    val waste'' = Word8Vector.fromList (List.concat (List.tabulate(padw*h,f)))
in BinIO.output(fh,waste''); BinIO.closeOut fh end
end

fun readBMP s =
    let val (w,h,_,t) = readbmp s
        fun pixel pos =
            channels (t pos)
    in (w,h, pixel) end

fun writeBMP (s, (w, h, pixel)) =
    let fun pixel' pos =
            let val (r,g,b) = pixel pos
            in r + g * 0x100 + b * 0x10000 end
        val c = Word8Vector.fromList []
    in writebmp (w,h,c,pixel') s end
end
