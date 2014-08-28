use "InstagraML.sml";

(* Drawing points *)
fun drawpoints img colour xs = 
      let val (w,h,f) = InstagraML.toFunction img
          fun overlaypoints p = 
              if List.exists (fn q => q = p) xs
              then colour
              else f p
      in InstagraML.fromFunction (w,h,overlaypoints)
      end

(* Line drawing *)
fun range (n,m) = List.tabulate (m-n, fn i => n+i)
fun iterate 0 f x = [x]
  | iterate n f x = x :: iterate (n-1) f (f x)

fun line ((x0, y0), (x1,y1)) =
  let val (dx,dy) = (x1-x0, y1-y0)
      fun next (y,D) = if D > 0
                       then (y+1, D + (2*dy-2*dx))
                       else (y,   D + 2*dy)
      val ls = iterate (x1-x0-1) next (y0, (2*dy - dx))
  in
      ListPair.zip (range (x0,x1), y0 :: map (#1) ls)
  end 

fun drawline img colour = drawpoints img colour o line;

(* Mercator projection *)
fun gudermann (longi0 : real) (lati, longi) = 
      (longi - longi0,
       Math.ln (Math.tan (Math.pi/4.0 + lati / 2.0)))

val cph_map = InstagraML.readBMP "cphmap.bmp";
