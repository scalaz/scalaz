package fjs

object F {
  implicit def ScalaFunction_F[A, B](ff: A => B) = new fj.F[A, B] {
    def f(a: A) = ff(a)
  }

  implicit def F_ScalaFunction[A, B](f: fj.F[A, B]) = f.f(_: A)
}
