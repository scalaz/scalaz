package fjs

object F3 {
  implicit def ScalaFunction3_F[A, B, C, D](ff: (A, B, C) => D) = new fj.F3[A, B, C, D] {
    def f(a: A, b: B, c: C) = ff(a, b, c)
  }

  implicit def F_ScalaFunction3[A, B, C, D](f: fj.F3[A, B, C, D]) = f.f(_: A, _: B, _: C)
}
