(* Drawing pixels *)

type shape = (int * int) -> bool

infix ++
infix --
infix &&

(* The shape resulting from overlaying sh1 and sh2 *)
fun sh1 ++ sh2 = (fn p => sh1 p orelse sh2 p)

(* The intersecting of the two shapes  *)
fun sh1 && sh2 = (fn p => sh1 p andalso sh2 p)

(* The shape resulting from subtracting sh2 from sh1 *)
fun sh1 -- sh2 = (fn p => sh1 p andalso not (sh2 p))

val union = foldl op++ (fn _ => false)
val intersect = foldl op&& (fn _ => true)
fun subtract x xs = foldl op-- x xs

val emptyShape = fn _ => false

(* Draw a shape (sh) on top of an image (img) in the specified
 * colour. *)
fun drawShape colour img sh = 
      let val (w,h,f) = InstagraML.toFunction img
          fun overlay p = if sh p then colour else f p
      in InstagraML.fromFunction (w,h, overlay)
      end

local
(* Construct a list by iterating value n times *)
fun iterate 0 f x = []
  | iterate n f x = x :: iterate (n-1) f (f x)

(* Line drawing algorithm: Digital differential analyzer *)
fun dda ((x0, y0), (x1, y1)) =
  if x0 > x1 then dda ((x1, y1), (x0, y0))
  else
    let val dx = abs(x1-x0)
        val dy = abs(y1-y0)
        val start = (real x0, real y0)

        fun line i next =
              map (fn (x,y) => (round x, round y))
                  (iterate i next start)

        fun steep slope (x,y) = (x + 1.0/slope, y + 1.0)
        fun flat slope (x,y) = (x + 1.0, y + slope)
        fun upwards (x,y) = (x, y + 1.0)
        fun downwards (x,y) = (x, y - 1.0)
    in 
        if dx = 0  (* slope = infinity  *)
        then if y0 > y1
             then line dy downwards
             else line dy upwards
        else
          let val slope = real (y1-y0) / real dx 
          in if abs slope <= 1.0
             then line dx (flat slope)
             else line dy (steep slope)
          end
    end

in
(* Create the line between points p1 and p2 as a shape *)
fun line (p1,p2) =
  let val points = dda (p1,p2)
  in 
      fn p => List.exists (fn q => q = p) points
  end
end;

fun drawLine c img (p1,p2) = drawShape c img (line (p1,p2))

fun lines xs = union (map line (ListPair.zip (xs, tl xs)))

(* Create a triangle from its three corner vertices *)
fun triangle (v1,v2,v3) = fn p =>
  let 
      fun sign ((x1, y1), (x2, y2), (x3,y3)) 
            = (x1 - x3) * (y2 - y3) -
              (x2 - x3) * (y1 - y3)
      val b1 = sign (p, v1, v2) < 0;
      val b2 = sign (p, v2, v3) < 0;
      val b3 = sign (p, v3, v1) < 0;
  in 
      b1 = b2 andalso b2 = b3
  end

(* Create a circle from radius and center point *)
fun circle ((a,b), r) = fn (x,y) =>
      let fun square x = x*x
      in  square (x-a) + square (y-b) < r
      end

fun rectangle ((x1,y1),(x2,y2)) = fn (p1,p2) => 
       Int.min (x1,x2) <= p1 andalso p1 <= Int.max(x1,x2) andalso
       Int.min (y1,y2) <= p2 andalso p2 <= Int.max(y1,y2)

(* fun polygon xs => fn p =>  *)

