package fjs

object P4 {
  implicit def P4_Tuple4[A, B, C, D](p: fj.P4[A, B, C, D]) = (p._1, p._2, p._3, p._4)

  implicit def Tuple4_P4[A, B, C, D](p: (A, B, C, D)) = fj.P.p(p._1, p._2, p._3, p._4)
}
