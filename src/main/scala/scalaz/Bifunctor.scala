package scalaz

trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](k: F[A, B], f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  implicit def Tuple2Bifunctor = new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](k: (A, B), f: A => C, g: B => D) =
      (f(k._1), g(k._2))
  }

  implicit def EitherBifunctor = new Bifunctor[Either] {
    def bimap[A, B, C, D](k: Either[A, B], f: A => C, g: B => D) =
      k match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }
}
