package scalaz
package data

trait TheseInstances {
  // implicit def bifunctor: Bifunctor[These] = ...

  implicit final def theseMonad[L: Semigroup]: Monad[These[L, ?]] =
    new Monad.Template[These[L, ?]] with Bind.DeriveAp[These[L, ?]] with Bind.DeriveFlatten[These[L, ?]] {
      override def map[A, B](ma: These[L, A])(f: A => B) = ma.rmap(f)
      def flatMap[A, B](ma: These[L, A])(f: A => These[L, B]) = ma.flatMap(f)
      def pure[A](a: A) = That(a)
    }

  implicit final def theseTraversable[L]: Traversable[These[L, ?]] =
    new Traversable.Template[These[L, ?]]
        with Traversable.DeriveSequence[These[L, ?]] with Foldable.DeriveToList[These[L, ?]] {
      def traverse[F[_]: Applicative, A, B](ta: These[L, A])(f: A => F[B]) = ta.traverse(f)
      def foldMap[A, B: Monoid](fa: These[L, A])(f: A => B) = fa.foldMap(f)
      def map[A, B](ma: These[L, A])(f: A => B) = ma.rmap(f)

      /* todo: remove these when default implementation in Traversable? */
      def foldRight[A, B](fa: These[L, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
      def foldLeft[A, B](fa: These[L, A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
    }

  implicit final def theseSemigroup[L: Semigroup, R: Semigroup]: Semigroup[These[L, R]] =
    new Semigroup.Template[These[L, R]] {
      def append(a1: These[L, R], a2: => These[L, R]) = a1.append(a2)
    }

  implicit final def theseShow[L, R](implicit L: Show[L], R: Show[R]): Show[These[L, R]] =
    new Show[These[L, R]] {
      def show(a: These[L, R]) =
        a.bimap(L.show)(R.show).toString
    }
}
