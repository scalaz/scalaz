package scalaz
package typeclass

import scala.language.experimental.macros

trait CobindSyntax {
  implicit final class ToCobindOps[F[_]: Cobind, A](fa: F[A]) {
    def cobind[B](f: F[A] => B): F[B] = macro meta.Ops.f_1
  }
}
