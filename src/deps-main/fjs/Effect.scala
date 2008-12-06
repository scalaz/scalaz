package fjs

object Effect {
  implicit def ScalaFunction_Effect[A](ff: A => Unit) = new fj.Effect[A] {
    def e(a: A) = ff(a)
  }

  implicit def Effect_ScalaFunction[A](f: fj.Effect[A]) = f.e(_: A)
}
