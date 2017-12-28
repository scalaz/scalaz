package scalaz
package typeclass

import scala.language.experimental.macros

trait TraversableSyntax {
  implicit final class ToTraversableOps[T[_]: Traversable, A](self: T[A]) {
    def traverse[F[_], B](f: A => F[B])(implicit g: Applicative[F]): F[T[B]] = macro meta.Ops.f_1_1
  }
}
