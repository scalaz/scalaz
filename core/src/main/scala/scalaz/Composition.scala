package scalaz

private[scalaz] trait CompositionFunctor[F[_], G[_]] extends Functor[({type λ[α] = F[G[α]]})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fga: F[G[A]])(f: (A) => B): F[G[B]] = F.map(fga)(ga => G.map(ga)(f))
}

private[scalaz] trait CompositionPointed[F[_], G[_]] extends Pointed[({type λ[α] = F[G[α]]})#λ] with CompositionFunctor[F, G] {
  implicit def F: Pointed[F]

  implicit def G: Pointed[G]

  def point[A](a: => A): F[G[A]] = F.point(G.point(a))
}

private[scalaz] trait CompositionApplicative[F[_], G[_]] extends Applicative[({type λ[α] = F[G[α]]})#λ] with CompositionPointed[F, G] {
  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  def ap[A, B](fa: => F[G[A]])(f: => F[G[A => B]]): F[G[B]] =
    F.map2(f, fa)((ff, ga) => G.ap(ga)(ff))
}
