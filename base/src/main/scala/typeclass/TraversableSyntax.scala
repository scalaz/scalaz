package scalaz
package typeclass

import scala.language.implicitConversions

trait TraversableSyntax {
  implicit def traversableOps[T[_], A](ta: T[A])(implicit T: Traversable[T]): TraversableSyntax.Ops[T, A] =
    new TraversableSyntax.Ops(ta)
}

object TraversableSyntax {
  class Ops[T[_], A](self: T[A])(implicit T: Traversable[T]) {
    def traverse[F[_], B](f: A => F[B])(implicit F: Applicative[F]): F[T[B]] =
      T.traverse(self)(f)
  }
}
