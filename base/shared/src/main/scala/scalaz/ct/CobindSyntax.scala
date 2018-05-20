package scalaz
package ct

import scala.language.experimental.macros

trait CobindSyntax {
  implicit final class ToCobindOps[F[_], A](fa: F[A]) {
    def cobind[B](f: F[A] => B)(implicit ev: Cobind[F]): F[B] = macro meta.Ops.i_1
  }
}
