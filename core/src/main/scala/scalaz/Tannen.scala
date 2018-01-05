package scalaz

/** Composes a Functor on the outside of a Bifunctor.
  *
  * [[https://hackage.haskell.org/package/bifunctors-5.5/docs/Data-Bifunctor-Tannen.html#t:Tannen]]
  */
final case class Tannen[F[_], G[_, _], A, B](f: F[G[A, B]])

object Tannen extends TannenInstances

sealed abstract class TannenInstances {
  implicit def TannenFunctor[F[_], G[_, _], E](implicit F: Functor[F], G: Bifunctor[G]): Functor[Tannen[F, G, E, ?]] =
    new Functor[Tannen[F, G, E, ?]] {
      def map[A, B](fab: Tannen[F, G, E, A])(f: A => B): Tannen[F, G, E, B] =
        Tannen(F.map(fab.f)(G.rightMap(_)(f)))
    }

  implicit def TannenBifunctor[F[_], G[_, _]](implicit F: Functor[F], G: Bifunctor[G]): Bifunctor[Tannen[F, G, ?, ?]] =
    new Bifunctor[Tannen[F, G, ?, ?]] {
      def bimap[A, B, C, D](fab: Tannen[F, G, A, B])(f: A => C, g: B => D): Tannen[F, G, C, D] =
        Tannen(F.map(fab.f)(G.bimap(_)(f, g)))
    }
}
