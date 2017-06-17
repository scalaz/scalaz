package scalaz
package typeclass

import scala.language.implicitConversions

trait ApplicativeSyntax {
  implicit def applicativeOpsA[A](a: A): ApplicativeSyntax.OpsA[A] = new ApplicativeSyntax.OpsA(a)
}

object ApplicativeSyntax {
  class OpsA[A](a: A) {
    def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
    def visit[F[_]](f: PartialFunction[A, F[A]])(implicit F: Applicative[F]) =
      f.applyOrElse[A, F[A]](a, _ => F.pure(a))
  }
}
