package scalaz

private[scalaz] trait CoproductFunctor[F[_], G[_]] extends Functor[({type λ[α] = Either[F[α], G[α]]})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](e: Either[F[A], G[A]])(f: (A) => B): Either[F[B], G[B]] =
    e match {
      case Left(fa) => Left(F.map(fa)(f))
      case Right(gb) => Right(G.map(gb)(f))
    }
}
