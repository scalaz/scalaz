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
    def mapConst[B](f: B): F[B] = macro meta.Ops._f1[B, F[B]]
    def >|[B](f: B): F[B] = macro meta.Ops._f1t[B, F[B], Functor.MapConst]
    def void: F[Unit] = F.mapConst(self)(())
  }
}


