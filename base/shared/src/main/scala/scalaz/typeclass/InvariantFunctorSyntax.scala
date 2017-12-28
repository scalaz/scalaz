package scalaz
package typeclass

import scala.language.experimental.macros

trait InvariantFunctorSyntax {
  implicit final class ToInvariantFunctorOps[F[_]: InvariantFunctor, A](self: F[A]) {
    def imap[B](f: A => B)(g: B => A): F[B] = macro meta.Ops.f_1_1
  }
}

