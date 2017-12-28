package scalaz
package typeclass

import scala.language.experimental.macros

trait FunctorSyntax {
  implicit final class ToFunctorOps[F[_], A](self: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = macro meta.Ops.f_1
    def void: F[Unit] = F.map[A, Unit](self)(_ => ())
  }
}

