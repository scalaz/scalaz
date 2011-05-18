package scalaz

trait Compose[F[_, _]] {
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]
}

object Compose extends Composes

trait Composes {

  implicit val Function1Compose: Compose[Function1] = new Compose[Function1] {
    def compose[A, B, C](f: B => C, g: A => B) =
      f compose g
  }

  implicit val PartialFunctionCompose: Compose[PartialFunction] = new Compose[PartialFunction] {
    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]) =
      new PartialFunction[A, C] {
        def isDefinedAt(a: A) = g.isDefinedAt(a) && f.isDefinedAt(g(a))

        def apply(a: A) = f(g(a))
      }
  }

  implicit val `<:<_Compose` : Compose[<:<] = new Compose[<:<] {
    def compose[A, B, C](f: <:<[B, C], g: <:<[A, B]) =
      f.asInstanceOf[A <:< C]
  }

  implicit val `=:=_Compose` : Compose[=:=] = new Compose[=:=] {
    def compose[A, B, C](f: =:=[B, C], g: =:=[A, B]) =
      f.asInstanceOf[A =:= C]
  }
}