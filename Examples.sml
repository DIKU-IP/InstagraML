use "InstagraML.sml";

(* The "Try" examples in comments below assumes the following file is
   loaded:  *)
val torben = InstagraML.readBMP "torben.bmp";

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

(* Try: InstagraML.writeBMP ("fisheye.bmp", (InstagraML.transform fish torben)); *)

(* Recolouring. *)
val invertColours = InstagraML.recolour (fn (r,g,b) => (255-r, 255-g, 255-b))

(* Green is perceived the most intense by the human eye, red next most
   intense and blue the least (Remark: this is a very primitive way to
   extract intensity, but it is efficient and easy to remember) *)
fun intensity (r,g,b) = (r+r+g+g+g+b) div 6
val greyscale = 
      InstagraML.recolour (fn x => let val v = intensity x in (v,v,v) end)

(* Andy Warhol-effect *)
fun warholEffect image colours =
  let val n = length colours
      val range = 256 div n
      fun intensity (r,g,b) = (r+g+b) div 3
      fun selectColour x = List.nth(colours, Int.min(x div range, n-1))
  in
      InstagraML.recolour (selectColour o intensity) image
  end

val black   = (0,0,0)
val white   = (255,255,255)
val red     = (255,0,0)
val green   = (0,255,0)
val blue    = (0,0,255)
val yellow  = (255,255,0)
val cyan    = (0,255,255)
val magenta = (255,0,255)
val orange  = (255,165,0)
val pink    = (255,192,203)
val purple  = (128,0,128)

val colours1 = [purple, pink, pink, cyan]
val colours2 = [blue, magenta, orange, yellow]
val colours3 = [red, red, yellow, green]
val colours4 = [black, red, red, white]

fun warhol img = four (warholEffect img colours1)
                      (warholEffect img colours2)
                      (warholEffect img colours3)
                      (warholEffect img colours4)

(* Try: InstagraML.writeBMP 
           ("warholspiral.bmp", 
            warhol (spiral (InstagraML.transform fish torben) 8));
 *)


fun quad img = four img img img img

(* Spiral fractal *)
fun spiral img 0 = img
  | spiral img n =
    let val a = InstagraML.scale 0.5 0.5 (quad img)
        val b = InstagraML.scale 0.5 0.5 (quad a)
    in InstagraML.beside
           (below img a)
           (InstagraML.clockwise (InstagraML.beside b (InstagraML.clockwise (spiral (InstagraML.scale 0.5 0.5 a) (n-1)))))
    end

(* Try: InstagraML.writeBMP ("spiral.bmp", spiral torben) 10); *)


(* Sepia tone filter

   Red Component: Sum total of: 18.9% blue, 76.9% green, 39.3% red
   Green Component: Sum total of: 16.8% blue, 68.6% green, 34.9% red
   Blue Component: Sum total of: 13.1% blue, 53.4% green, 27.2% red
*)

fun sepiaColor (r,g,b) =
      let open Real;
          val br = fromInt r
          val bg = fromInt g
          val bb = fromInt b
          val rsepia = round (0.189 * bb + 0.769 * bg + 0.393 * br)
          val gsepia = round (0.168 * bb + 0.686 * bg + 0.349 * br)
          val bsepia = round (0.131 * bb + 0.534 * bg + 0.272 * br)
      in
          (Int.min (255, rsepia),
           Int.min (255, gsepia),
           Int.min (255, bsepia))
      end;

val sepia = InstagraML.recolour sepiaColor;

(* Try: InstagraML.writeBMP ("sepia.bmp", sepia torben); *)


(* Mandelbrot fractal *)
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
end
