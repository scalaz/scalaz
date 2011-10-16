package scalaz

trait Compose[~>:[_, _]] {
  ////
  def compose[A, B, C](f: B ~>: C, g: A ~>: B): (A ~>: C)

  ////
}

object Compose {
  ////

  //
  //  implicit val Function1Compose: Compose[Function1] = new Compose[Function1] {
  //    def compose[A, B, C](f: B => C, g: A => B) =
  //      f compose g
  //  }
  //
  //  implicit val PartialFunctionCompose: Compose[PartialFunction] = new Compose[PartialFunction] {
  //    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]) =
  //      new PartialFunction[A, C] {
  //        def isDefinedAt(a: A) = g.isDefinedAt(a) && f.isDefinedAt(g(a))
  //
  //        def apply(a: A) = f(g(a))
  //      }
  //  }
  //
  //  implicit val `<:<_Compose` : Compose[<:<] = new Compose[<:<] {
  //    def compose[A, B, C](f: <:<[B, C], g: <:<[A, B]) =
  //      f.asInstanceOf[A <:< C]
  //  }
  //
  //  implicit val `=:=_Compose` : Compose[=:=] = new Compose[=:=] {
  //    def compose[A, B, C](f: =:=[B, C], g: =:=[A, B]) =
  //      f.asInstanceOf[A =:= C]
  //  }
  //
  //  implicit def KleisliCompose[F[_]](implicit bd: Bind[F]): Compose[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new Compose[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
  //    def compose[A, B, C](f: Kleisli[B, F, C], g: Kleisli[A, F, B]) =
  //      f <=< g
  //  }
  //
  //  implicit def CokleisliCompose[F[_]](implicit ex: Extend[F]): Compose[({type λ[α, β] = Cokleisli[α, F, β]})#λ] = new Compose[({type λ[α, β] = Cokleisli[α, F, β]})#λ] {
  //    def compose[A, B, C](f: Cokleisli[B, F, C], g: Cokleisli[A, F, B]) =
  //      f =<= g
  //  }
  //
  //  implicit def LensCompose: Compose[Lens] = new Compose[Lens] {
  //    def compose[A, B, C](f: Lens[B, C], g: Lens[A, B]) =
  //      f >=> g
  //  }

  ////
}
