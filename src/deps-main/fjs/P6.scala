package fjs

object P6 {
  implicit def P6_Tuple6[A, B, C, D, E, F$](p: fj.P6[A, B, C, D, E, F$]) = (p._1, p._2, p._3, p._4, p._5, p._6)

  implicit def Tuple6_P6[A, B, C, D, E, F$](p: (A, B, C, D, E, F$)) = fj.P.p(p._1, p._2, p._3, p._4, p._5, p._6)
}
