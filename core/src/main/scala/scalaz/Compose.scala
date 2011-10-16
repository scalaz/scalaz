package scalaz

trait Compose[=>:[_, _]]  { self =>
  ////
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)

  ////
  val composeSyntax = new scalaz.syntax.ComposeSyntax[=>:] {}
}

object Compose {
  def apply[F[_, _]](implicit F: Compose[F]): Compose[F] = F

  ////
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

