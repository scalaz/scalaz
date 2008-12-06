package fjs

object P8 {
  implicit def P8_Tuple8[A, B, C, D, E, F$, G, H](p: fj.P8[A, B, C, D, E, F$, G, H]) = (p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8)

  implicit def Tuple8_P8[A, B, C, D, E, F$, G, H](p: (A, B, C, D, E, F$, G, H)) = fj.P.p(p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8)
}
