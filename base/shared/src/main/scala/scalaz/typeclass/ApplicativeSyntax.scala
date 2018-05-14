package scalaz
package typeclass

import language.experimental.macros

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = macro meta.Ops.i_0
  }
}
