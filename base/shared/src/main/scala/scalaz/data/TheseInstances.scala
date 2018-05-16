package scalaz
package data

import scalaz.algebra.SemigroupClass
import scalaz.ct._
import scalaz.typeclass._

trait TheseInstances {
  implicit def bifunctor: Bifunctor[These] =
    instanceOf(new BifunctorClass[These] {
      def bimap[A, B, S, T](fab: These[A, B])(as: A => S, bt: B => T) = fab.bimap(as)(bt)

      def lmap[A, B, S](fab: These[A, B])(as: A => S) = fab.lmap(as)
      def rmap[A, B, T](fab: These[A, B])(bt: B => T) = fab.rmap(bt)
    })

  implicit final def theseMonad[L: Semigroup]: Monad[These[L, ?]] =
    instanceOf(
      new MonadClass[These[L, ?]] with BindClass.DeriveAp[These[L, ?]] with BindClass.DeriveFlatten[These[L, ?]] {
        override def map[A, B](ma: These[L, A])(f: A => B)      = ma.rmap(f)
        def flatMap[A, B](ma: These[L, A])(f: A => These[L, B]) = ma.flatMap(f)
        def pure[A](a: A)                                       = That(a)
      }
    )

  implicit final def theseTraversable[L]: Traversable[These[L, ?]] =
    instanceOf(new TraversableClass.DeriveSequence[These[L, ?]] with FoldableClass.DeriveToList[These[L, ?]] {
      def traverse[F[_]: Applicative, A, B](ta: These[L, A])(f: A => F[B]) = ta.traverse(f)
      def foldMap[A, B: Monoid](fa: These[L, A])(f: A => B)                = fa.foldMap(f)
      def map[A, B](ma: These[L, A])(f: A => B)                            = ma.rmap(f)

      /* todo: remove these when default implementation in Traversable? */
      def foldRight[A, B](fa: These[L, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
      def foldLeft[A, B](fa: These[L, A], z: B)(f: (B, A) => B)        = fa.foldLeft(z)(f)
    })

  implicit final def theseSemigroup[L: Semigroup, R: Semigroup]: Semigroup[These[L, R]] =
    instanceOf[SemigroupClass[These[L, R]]](
      _.append(_)
    )

  implicit final def theseDebug[L, R](implicit L: Debug[L], R: Debug[R]): Debug[These[L, R]] =
    instanceOf[DebugClass[These[L, R]]](
      _.bimap(L.debug)(R.debug).toString
    )

  implicit final def theseEq[L, R](implicit L: Eq[L], R: Eq[R]): Eq[These[L, R]] =
    instanceOf[EqClass[These[L, R]]] {
      case (This(l1), This(l2))         => L.equal(l1, l2)
      case (That(r1), That(r2))         => R.equal(r1, r2)
      case (Both(l1, r1), Both(l2, r2)) => L.equal(l1, l2) && R.equal(r1, r2)
      case _                            => false
    }
}
