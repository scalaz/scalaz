package fjs

object P2 {
  implicit def P2_Tuple2[A, B](p: fj.P2[A, B]) = (p._1, p._2)

  implicit def Tuple2_P2[A, B](p: (A, B)) = fj.P.p(p._1, p._2)
}
