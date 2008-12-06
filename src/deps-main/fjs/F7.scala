package fjs

object F7 {
  implicit def ScalaFunction7_F[A, B, C, D, E, F$, G, H](ff: (A, B, C, D, E, F$, G) => H) = new fj.F7[A, B, C, D, E, F$, G, H] {
    def f(a: A, b: B, c: C, d: D, e: E, f: F$, g: G) = ff(a, b, c, d, e, f, g)
  }

  implicit def F_ScalaFunction7[A, B, C, D, E, F$, G, H](f: fj.F7[A, B, C, D, E, F$, G, H]) = f.f(_: A, _: B, _: C, _: D, _: E, _: F$, _: G)  
}
