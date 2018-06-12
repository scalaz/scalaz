package scalaz
package ct

import scala.language.experimental.macros

trait ComonadClass[F[_]] extends CobindClass[F] {
  def copoint[A](fa: F[A]): A
}

trait ComonadSyntax {
  implicit final class ToComonadOps[F[_], A](self: F[A]) {
    def copoint(implicit ev: Comonad[F]): A = macro meta.Ops.i_0
  }
}
