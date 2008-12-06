package fjs

object F5 {
  implicit def ScalaFunction5_F[A, B, C, D, E, F$](ff: (A, B, C, D, E) => F$) = new fj.F5[A, B, C, D, E, F$] {
    def f(a: A, b: B, c: C, d: D, e: E) = ff(a, b, c, d, e)
  }

  implicit def F_ScalaFunction5[A, B, C, D, E, F$](f: fj.F5[A, B, C, D, E, F$]) = f.f(_: A, _: B, _: C, _: D, _: E)
}
