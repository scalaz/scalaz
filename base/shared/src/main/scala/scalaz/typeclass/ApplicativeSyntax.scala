package scalaz
package typeclass

import language.experimental.macros

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]: Applicative]: F[A] = macro meta.IdOps.id_1
  }
}
