package scalaz
package typeclass

import scala.language.experimental.macros

trait ComonadSyntax {
  implicit final class ToComonadOps[F[_], A](self: F[A]) {
    def copoint(implicit ev: Comonad[F]): A = macro meta.Ops.i_0
  }
}
