package fjs

object P3 {
  implicit def P3_Tuple3[A, B, C](p: fj.P3[A, B, C]) = (p._1, p._2, p._3)

  implicit def Tuple3_P3[A, B, C](p: (A, B, C)) = fj.P.p(p._1, p._2, p._3)  
}
