package scalaz
package ct

import scala.language.experimental.macros

trait TraversableClass[T[_]] extends FunctorClass[T] with FoldableClass[T] {

  def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]]

  def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]]
}

object TraversableClass {

  trait DeriveSequence[T[_]] extends TraversableClass[T] with Alt[DeriveSequence[T]] {
    final override def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]] = traverse(ta)(identity)
  }

  trait DeriveTraverse[T[_]] extends TraversableClass[T] with Alt[DeriveTraverse[T]] {
    final override def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]] = sequence(map(ta)(f))
  }

  trait Alt[D <: Alt[D]]
}

trait TraversableFunctions {
  def sequence[T[_], F[_], A](tfa: T[F[A]])(implicit F: Applicative[F], T: Traversable[T]): F[T[A]] =
    T.sequence(tfa)
}

trait TraversableSyntax {
  implicit final class ToTraversableOps[T[_], A](self: T[A]) {
    def traverse[F[_], B](f: A => F[B])(implicit g: Applicative[F], ev: Traversable[T]): F[T[B]] =
      macro meta.Ops.i_1_1i
  }
}
