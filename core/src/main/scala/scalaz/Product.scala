package scalaz


private[scalaz] trait ProductFunctor[F[_], G[_]] extends Functor[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) = (F.map(fa._1)(f), G.map(fa._2)(f))
}


private[scalaz] trait ProductPointed[F[_], G[_]] extends Pointed[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] {
  implicit def F: Pointed[F]

  implicit def G: Pointed[G]

  def pure[A](a: => A): (F[A], G[A]) = (F.pure(a), G.pure(a))
}

private[scalaz] trait ProductApplicative[F[_], G[_]] extends Applicative[({type λ[α] = (F[α], G[α])})#λ] with ProductPointed[F, G] {
  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  def ap[A, B](fa: (F[A], G[A]))(f: (F[(A) => B], G[(A) => B])): (F[B], G[B]) = (F.ap(fa._1)(f._1), G.ap(fa._2)(f._2))
}

