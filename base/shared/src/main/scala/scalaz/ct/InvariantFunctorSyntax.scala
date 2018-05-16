package scalaz
package ct

import scala.language.experimental.macros

trait InvariantFunctorSyntax {
  implicit final class ToInvariantFunctorOps[F[_], A](self: F[A]) {
    def imap[B](f: A => B)(g: B => A)(implicit ev: InvariantFunctor[F]): F[B] = macro meta.Ops.i_1_1
  }
}
