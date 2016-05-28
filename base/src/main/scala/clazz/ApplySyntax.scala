package scalaz
package clazz

import scala.language.implicitConversions

trait ApplySyntax {
  implicit def applyOps[F[_], A](fa: F[A])(implicit F: Apply[F]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)
}

object ApplySyntax {
  class Ops[F[_], A](fa: F[A])(implicit F: Apply[F]) {
    def ap[B](fab: F[A => B]): F[B] = F.ap(fa)(fab)
  }
}
