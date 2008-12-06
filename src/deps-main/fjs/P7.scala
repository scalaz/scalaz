package fjs

object P7 {
  implicit def P7_Tuple7[A, B, C, D, E, F$, G](p: fj.P7[A, B, C, D, E, F$, G]) = (p._1, p._2, p._3, p._4, p._5, p._6, p._7)

  implicit def Tuple7_P7[A, B, C, D, E, F$, G](p: (A, B, C, D, E, F$, G)) = fj.P.p(p._1, p._2, p._3, p._4, p._5, p._6, p._7)
}
