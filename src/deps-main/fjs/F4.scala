package fjs

object F4 {
  implicit def ScalaFunction4_F[A, B, C, D, E](ff: (A, B, C, D) => E) = new fj.F4[A, B, C, D, E] {
    def f(a: A, b: B, c: C, d: D) = ff(a, b, c, d)
  }

  implicit def F_ScalaFunction4[A, B, C, D, E](f: fj.F4[A, B, C, D, E]) = f.f(_: A, _: B, _: C, _: D)  
}
