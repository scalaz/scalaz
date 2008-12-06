package fjs

object F2 {
  implicit def ScalaFunction2_F[A, B, C](ff: (A, B) => C) = new fj.F2[A, B, C] {
    def f(a: A, b: B) = ff(a, b)
  }

  implicit def F_ScalaFunction2[A, B, C](f: fj.F2[A, B, C]) = f.f(_: A, _: B)
}
