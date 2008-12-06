package fjs

object P5 {
  implicit def P5_Tuple5[A, B, C, D, E](p: fj.P5[A, B, C, D, E]) = (p._1, p._2, p._3, p._4, p._5)

  implicit def Tuple5_P5[A, B, C, D, E](p: (A, B, C, D, E)) = fj.P.p(p._1, p._2, p._3, p._4, p._5)  
}
