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

  implicit def KleisliCompose[F[_]](implicit bd: Bind[F]): Compose[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new Compose[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
    def compose[A, B, C](f: Kleisli[B, F, C], g: Kleisli[A, F, B]) =
      f <=< g
  }

  implicit def CoKleisliCompose[F[_]](implicit ex: Extend[F]): Compose[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = new Compose[({type λ[α, β] = CoKleisli[α, F, β]})#λ] {
    def compose[A, B, C](f: CoKleisli[B, F, C], g: CoKleisli[A, F, B]) =
      f =<= g
  }

  implicit def LensCompose: Compose[Lens] = new Compose[Lens] {
    def compose[A, B, C](f: Lens[B, C], g: Lens[A, B]) =
      f >=> g
  }

}