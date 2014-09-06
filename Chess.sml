use "InstagraML.sml";

(* From: http://codegolf.stackexchange.com/questions/35569/tweetable-mathematical-art#35641 *)

fun rep3 v = (v,v,v)

fun chess idim = 
  let
      val dim = Real.fromInt idim
      fun chess' (ii, ij) =
          let val (i,j) = (Real.fromInt ii, Real.fromInt (idim-ij))
              val s = 3.0/(j+99.0)
          in (Real.round((i+dim)*s+j*s) mod 2 + 
              Real.round((dim*2.0-i)*s+j*s) mod 2)*127
          end
  in 
      InstagraML.fromFunction (idim, idim, rep3 o chess')
  end

fun ripple idim =
  let
      val dim = Real.fromInt idim
      fun ripple' (ii,ij) =
          let val (i,j) = (Real.fromInt ii, Real.fromInt (idim-ij))
              val s = 3.0/(j+99.0)
              fun sq x = x*x
              val y = (j+Math.sin((i*i+sq(j-700.0)*5.0)/100.0/dim)*35.0)*s
          in (Real.round((i+dim)*s+y) mod 2 +
              Real.round((dim*2.0-i)*s+y) mod 2)*127
      end
  in 
      InstagraML.fromFunction (idim, idim, rep3 o ripple')
  end


val _ = InstagraML.writeBMP ("chess.bmp", chess 1024);
val _ = InstagraML.writeBMP ("ripple.bmp", ripple 1024);

