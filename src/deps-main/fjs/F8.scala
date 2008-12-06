package fjs

object F8 {
  implicit def ScalaFunction8_F[A, B, C, D, E, F$, G, H, I](ff: (A, B, C, D, E, F$, G, H) => I) = new fj.F8[A, B, C, D, E, F$, G, H, I] {
    def f(a: A, b: B, c: C, d: D, e: E, f: F$, g: G, h: H) = ff(a, b, c, d, e, f, g, h)
  }

  implicit def F_ScalaFunction8[A, B, C, D, E, F$, G, H, I](f: fj.F8[A, B, C, D, E, F$, G, H, I]) = f.f(_: A, _: B, _: C, _: D, _: E, _: F$, _: G, _: H)    
}
