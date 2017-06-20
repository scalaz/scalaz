package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.implicitConversions
import scala.language.experimental.macros

trait TraversableSyntax {
  implicit def traversableOps[T[_], A](ta: T[A])(implicit T: Traversable[T]): TraversableSyntax.Ops[T, A] =
    new TraversableSyntax.Ops(ta)
}

object TraversableSyntax {
  class Ops[T[_], A](self: T[A])(implicit T: Traversable[T]) {
    def traverse[F[_], B](f: A => F[B])(implicit g: Applicative[F]): F[T[B]] = macro meta.Ops._f2[A => F[B], Applicative[F], F[T[B]]]
  }
}
