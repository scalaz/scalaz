package scalaz

/** Composes a Functor on the outside of a Bifunctor.
  *
  * [[https://hackage.haskell.org/package/bifunctors-5.5/docs/Data-Bifunctor-Tannen.html#t:Tannen]]
  */
final case class Tannen[F[_], G[_, _], A, B](f: F[G[A, B]])

object Tannen extends TannenInstances

sealed abstract class TannenInstances0 {
  implicit def TannenFoldable[F[_], G[_, _], E](implicit F: Foldable[F], G: Bifoldable[G]): Foldable[Tannen[F, G, E, *]] =
    new Foldable[Tannen[F, G, E, *]] {
      def foldMap[A, M](fa: Tannen[F, G, E, A])(f: A => M)(implicit M: Monoid[M]): M =
        F.foldMap(fa.f)(G.bifoldMap(_)(_ => M.zero)(f))
      def foldRight[A, B](fa: Tannen[F, G, E, A], z: => B)(f: (A, => B) => B): B =
        F.foldRight(fa.f, z)((a, b) => G.bifoldRight(a, b)((_, _) => b)(f))
    }

  implicit def TannenFunctor[F[_], G[_, _], E](implicit F: Functor[F], G: Bifunctor[G]): Functor[Tannen[F, G, E, *]] =
    new Functor[Tannen[F, G, E, *]] {
      def map[A, B](fab: Tannen[F, G, E, A])(f: A => B): Tannen[F, G, E, B] =
        Tannen(F.map(fab.f)(G.rightMap(_)(f)))
    }

  implicit def TannenBifoldable[F[_], G[_, _]](implicit F: Foldable[F], G: Bifoldable[G]): Bifoldable[Tannen[F, G, *, *]] =
    new Bifoldable[Tannen[F, G, *, *]] {
      def bifoldMap[A, B, M](fa: Tannen[F, G, A, B])(f: A => M)(g: B => M)(implicit M: Monoid[M]): M =
        F.foldMap(fa.f)(G.bifoldMap(_)(f)(g))
      def bifoldRight[A, B, C](fa: scalaz.Tannen[F, G, A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
        F.foldRight(fa.f, z)(G.bifoldRight(_, _)(f)(g))
    }

  implicit def TannenBifunctor[F[_], G[_, _]](implicit F: Functor[F], G: Bifunctor[G]): Bifunctor[Tannen[F, G, *, *]] =
    new Bifunctor[Tannen[F, G, *, *]] {
      def bimap[A, B, C, D](fab: Tannen[F, G, A, B])(f: A => C, g: B => D): Tannen[F, G, C, D] =
        Tannen(F.map(fab.f)(G.bimap(_)(f, g)))
    }
}

sealed abstract class TannenInstances extends TannenInstances0 {
  implicit def TannenTraverse[F[_], G[_, _], E](implicit F: Traverse[F], G: Bitraverse[G]): Traverse[Tannen[F, G, E, *]] =
    new Traverse[Tannen[F, G, E, *]] {
      def traverseImpl[H[_], A, B](fa: Tannen[F, G, E, A])(f: A => H[B])(implicit H: Applicative[H]): H[Tannen[F, G, E, B]] =
        H.map(F.traverse(fa.f)(G.bitraverse(_)(H.pure(_))(f)))(Tannen.apply)
    }

  implicit def TannenBitraverse[F[_], G[_, _]](implicit F: Traverse[F], G: Bitraverse[G]): Bitraverse[Tannen[F, G, *, *]] =
    new Bitraverse[Tannen[F, G, *, *]] {
      def bitraverseImpl[H[_], A, B, C, D](fab: Tannen[F, G, A, B])(f: A => H[C], g: B => H[D])(implicit H: Applicative[H]): H[Tannen[F, G, C, D]] =
        H.map(F.traverse(fab.f)(G.bitraverse(_)(f)(g)))(Tannen.apply)
    }

  implicit def TannenEqual[F[_], G[_, _], A, B](implicit F: Equal[F[G[A, B]]]): Equal[Tannen[F, G, A, B]] =
    F.contramap(_.f)
}
