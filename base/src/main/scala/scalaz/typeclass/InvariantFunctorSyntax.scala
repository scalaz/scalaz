package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait InvariantFunctorSyntax {
  implicit def invariantFunctorOps[F[_], A](fa: F[A])(implicit F: InvariantFunctor[F]): InvariantFunctorSyntax.Ops[F, A] =
    new InvariantFunctorSyntax.Ops(fa)
}

object InvariantFunctorSyntax {
  class Ops[F[_], A](self: F[A])(implicit F: InvariantFunctor[F]) {
    def imap[B](f: A => B)(g: B => A): F[B] = macro meta.Ops._f2[A => B, B => A, F[B]]
  }
}

