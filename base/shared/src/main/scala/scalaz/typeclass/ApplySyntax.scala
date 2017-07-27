package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait ApplySyntax {
  implicit def applyOps[F[_], A](fa: F[A])(implicit F: Apply[F]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)
}

object ApplySyntax {
  class Ops[F[_], A](fa: F[A])(implicit F: Apply[F]) {
    def ap[B](f: F[A => B]): F[B] = macro meta.Ops._f1[F[A => B], F[B]]
  }
}
