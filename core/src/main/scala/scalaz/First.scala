package scalaz

trait First[F[_, _]] {
  def first[A, B, C](f: F[A, B]): F[(A, C), (B, C)]
}

object First extends Firsts

trait Firsts {
  implicit val Function1First: First[Function1] = new First[Function1] {
    def first[A, B, C](f: A => B) =
      ac => (f(ac._1), ac._2)
  }

  implicit val PartialFunctionFirst: First[PartialFunction] = new First[PartialFunction] {
    def first[A, B, C](f: PartialFunction[A, B]) = {
      case (a, c) if f isDefinedAt a => (f(a), c)
    }
  }

  implicit def KleisliFirst[F[_]](implicit ftr: Functor[F]): First[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new First[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
    def first[A, B, C](f: Kleisli[A, F, B]) =
      Kleisli.kleisli[(A, C), F, (B, C)] {
        case (a, c) => ftr.fmap((b: B) => (b, c))(f.run(a))
      }
  }

  implicit def CoKleisliFirst[F[_]](implicit ftr: CoPointedFunctor[F]): First[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = new First[({type λ[α, β] = CoKleisli[α, F, β]})#λ] {
    def first[A, B, C](f: CoKleisli[A, F, B]) =
      CoKleisli.coKleisli[(A, C), F, (B, C)](w =>
        (f.run(ftr.fmap((ac: (A, C)) => ac._1)(w)), ftr.coPoint(w)._2))
  }
}