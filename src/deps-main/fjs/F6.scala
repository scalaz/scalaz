package fjs

object F6 {
  implicit def ScalaFunction6_F[A, B, C, D, E, F$, G](ff: (A, B, C, D, E, F$) => G) = new fj.F6[A, B, C, D, E, F$, G] {
    def f(a: A, b: B, c: C, d: D, e: E, f: F$) = ff(a, b, c, d, e, f)
  }

  implicit def F_ScalaFunction6[A, B, C, D, E, F$, G](f: fj.F6[A, B, C, D, E, F$, G]) = f.f(_: A, _: B, _: C, _: D, _: E, _: F$)
}
