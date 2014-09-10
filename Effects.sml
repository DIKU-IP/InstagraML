(* Diverse effekter til InstagraML. Kræver at InstagraML er indlæst
 * først. *)

(* Sepia tone filter
 *
 *     sepia : image -> image
 *)
local
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
    end
in
  val sepia = InstagraML.recolour sepiaColor
end

(* Use these with InstagraML.transform:
 *   Example: "InstagraML.transform (rotate (Math.pi/4.0)) image"
 *)
fun zoom (x,y)     = (x*0.5,y*0.5);
fun fish (x,y)     = let val r = Math.sqrt(x*x+y*y) in (r*x,r*y) end
fun rotate a (x,y) = let val c = Math.cos a
                         val s = Math.sin a
                     in (x*c-y*s, x*s+y*c) end
fun unfish p (x,y) = let val r = (Math.sqrt(x*x+y*y)+p)/(1.0+p) in (x/r, y/r) end
fun whirl (x,y)    = let val r = Math.sqrt(x*x+y*y) in rotate (1.0-r) (x,y) end

(* Invert colours *)
val invertColours = InstagraML.recolour (fn (r,g,b) => (255-r, 255-g, 255-b))

(* Calculate the intensity of a colour.
 *
 *   intensity : colour -> int
 *
 * Green is perceived the most intense by the human eye, red next most
 * intense and blue the least (Remark: this is a very primitive way to
 * extract intensity, but it is efficient and easy to remember)
 *)
fun intensity (r,g,b) = (r+r+g+g+g+b) div 6

(* Greyscale version of an image *)
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
  end;

(* Example: 
    warholEffect img [RGB.blue, RGB.magenta, RGB.orange, RGB.yellow] *)


(* Spiral fractal of an image, diminishing in size. The integer
 * parameter controls how many times to repeat, should be at least "2"
 * for good effect.
 *
 * The dimensions of the output is double the dimensions of the input.
 *
 * The function is on purpose a bit hard to read, to obfuscate some basic
 * functionality.
 *)
local
  fun quad img = 
        let val x = InstagraML.clockwise (InstagraML.beside (img, img))
        in InstagraML.beside (x, x) end
in
fun spiral img 0 = img
  | spiral img n =
    let val a = InstagraML.scale 0.5 0.5 (quad img)
        val b = InstagraML.scale 0.5 0.5 (quad a)
    in InstagraML.beside
           ((InstagraML.clockwise 
               (InstagraML.clockwise 
                  (InstagraML.clockwise (InstagraML.beside (img, a))))),
           (InstagraML.clockwise 
              (InstagraML.beside 
                 (b, (InstagraML.clockwise 
                        (spiral (InstagraML.scale 0.5 0.5 a) (n-1)))))))
    end
end;
