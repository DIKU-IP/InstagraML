;use "InstagraML.sml";

functor ConvolutionFn (I : IMAGE) =
struct
  open I

  exception Dim

  local
    fun extractRegion img (x, y) n =
      let val offset = n div 2
          val (w,h,f) = I.toFunction img
      in
        List.tabulate (n, fn i =>
          List.tabulate (n, fn j =>
            let val ri = Int.min (x-offset+i, w-1)
                val rj = Int.min (y-offset+j, h-1) in
              f (ri, rj)
            end
          )
        )
      end

    infix 7 ** //
    infix 5 ++
    fun (a,b,c) ** s = (a*s, b*s, c*s)
    fun (a,b,c) // 0.0 = (a,b,c)
      | (a,b,c) // s = (a/s, b/s, c/s)
    fun (a,b,c) ++ (d,e,f) = (a+d, b+e, c+f)

    fun realify (a,b,c) = (real a, real b, real c)
    local fun to8I x = Int.max(Int.min(Real.round x, 255), 0) in
      fun intify (a,b,c) = (to8I a, to8I b, to8I c)
    end

    fun doConvolve ([]::xs) ([]::ys) v w = doConvolve xs ys v w
      | doConvolve ((x::xs')::xs) ((y::ys')::ys) v w =
          doConvolve (xs'::xs) (ys'::ys) (v ++ x**y) (w + y)
      | doConvolve [] [] v w = intify (v // w)
      | doConvolve _ _ _ _ = raise Dim
  in
    fun convolve img kernel =
      let val n = length kernel in
        I.fromFunction
        (I.width img, I.height img,
         (fn (i, j) =>
             doConvolve (map (map realify) (extractRegion img (i, j) n))
                        kernel (0.0, 0.0, 0.0) 0.0))
      end
  end
end

structure I = ConvolutionFn(DelayedImage);

val gaussianBlur = [[1.0,2.0,1.0],[2.0,4.0,2.0],[1.0,2.0,1.0]];
val boxBlur = [[1.0,1.0,1.0],[1.0,1.0,1.0],[1.0,1.0,1.0]];
val emboss = [[~2.0,~1.0,0.0],[~1.0,1.0,1.0],[0.0,1.0,2.0]];
val highPass = [[~1.0, ~1.0, ~1.0],[~1.0,9.0,~1.0],[~1.0,~1.0,~1.0]];
fun motionBlur n =
    List.tabulate(n, fn i => List.tabulate (n, fn j =>
      if n-i = j then 1.0 else 0.0
    ))

val torben = InstagraML.readBMP "torben.bmp";
val blurredtorben = I.convolve torben gaussianBlur;
val _ = InstagraML.writeBMP ("torbenblur.bmp", blurredtorben);
val embossedtorben = I.convolve torben emboss;
val _ = InstagraML.writeBMP ("torbenemboss.bmp", embossedtorben);
val edgytorben = I.convolve torben highPass;
val _ = InstagraML.writeBMP ("torbenedgy.bmp", edgytorben);
val speedytorben = I.convolve torben (motionBlur 9);
val _ = InstagraML.writeBMP ("torbenfasttorbenfurious.bmp", speedytorben);
