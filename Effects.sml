(*********************)
(**  Misc. Effects  **)
(*********************)

(* Sepia tone filter
 *
 *  sepia : image -> image
 *
 *  Try: InstagraML.writeBMP ("sepia.bmp", sepia image);
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


(* Some functions to be used together with InstagraML.transform: *)
fun zoom (x,y)     = (x*0.5,y*0.5);
fun fish (x,y)     = let val r = Math.sqrt(x*x+y*y) in (r*x,r*y) end
fun rotate a (x,y) = let val c = Math.cos a
                         val s = Math.sin a
                     in (x*c-y*s, x*s+y*c) end
fun unfish p (x,y) = let val r = (Math.sqrt(x*x+y*y)+p)/(1.0+p) in (x/r, y/r) end
fun whirl (x,y)    = let val r = Math.sqrt(x*x+y*y) in rotate (1.0-r) (x,y) end


(* Some funky effects using InstagraML.transform
 *   Try: zoomEffect image
 *   Try: fishEffect image
 *   Try: rotate45degrees image
 *)

val zoomEffect  = InstagraML.transform zoom;
val fishEffect  = InstagraML.transform fish;
val whirlEffect = InstagraML.transform whirl;
val rotate45degrees = InstagraML.transform (rotate (Math.pi/4.0));

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

(* Greyscale version of an image
 *
 *  greyscale : image -> image
 *
 *  Try: InstagraML.writeBMP ("greyscale.bmp", greyscale image);
 *)
val greyscale = 
      InstagraML.recolour (fn x => let val v = intensity x in (v,v,v) end)

(* Andy Warhol-effect
 *
 * Try: 
 *   warholEffect img [RGB.blue, RGB.magenta, RGB.orange, RGB.yellow]
 *)
fun warholEffect image colours =
  let val n = length colours
      val range = 256 div n
      fun intensity (r,g,b) = (r+g+b) div 3
      fun selectColour x = List.nth(colours, Int.min(x div range, n-1))
  in
      InstagraML.recolour (selectColour o intensity) image
  end;

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
    (* Open InstagraML-structure so we don't need to prefix everything
     * with "InstagraML." *)
    open InstagraML;
    fun quad img = 
      let val x = clockwise (beside (img, img))
      in beside (x, x) end
in

(* Spiral fractal *)
fun spiral img 0 = quad (scale 0.5 0.5 (quad img))
  | spiral img n =
    let val a = scale 0.5 0.5 (quad img)
        val b = scale 0.5 0.5 (quad a)
    in InstagraML.beside
           ((clockwise o clockwise o clockwise o beside) (clockwise img, clockwise a),
           ((clockwise o clockwise o clockwise o beside) ((clockwise o clockwise o clockwise) 
                                                              (spiral (InstagraML.scale 0.5 0.5 a) (n-1)), clockwise b)))
    end
end;
