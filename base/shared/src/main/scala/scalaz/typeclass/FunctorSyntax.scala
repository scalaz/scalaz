package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait FunctorSyntax {
  implicit def functorOps[F[_], A](fa: F[A])(implicit F: Functor[F]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)
}

object FunctorSyntax {
  class Ops[F[_], A](self: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = macro meta.Ops._f1[A => B, F[B]]
    def void: F[Unit] = F.map[A, Unit](self)(_ => ())
  }
}


