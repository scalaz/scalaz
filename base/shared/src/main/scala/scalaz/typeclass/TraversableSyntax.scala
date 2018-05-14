package scalaz
package typeclass

import scala.language.experimental.macros

trait TraversableSyntax {
  implicit final class ToTraversableOps[T[_], A](self: T[A]) {
    def traverse[F[_], B](f: A => F[B])(implicit g: Applicative[F], ev: Traversable[T]): F[T[B]] =
      macro meta.Ops.i_1_1i
  }
}
