package fjs

object P1 {
  implicit def P1_Tuple1[A](p: fj.P1[A]) = Tuple1(p._1)

  implicit def Tuple1_P1[A](p: Tuple1[A]) = fj.P.p(p._1)

  implicit def ScalaFunction_P1[A](f: Function0[A]): fj.P1[A] = new fj.P1[A] {
    def _1 = f()
  }

  implicit def P1_ScalaFunction[A](p: fj.P1[A]) = (() => p._1)

}
