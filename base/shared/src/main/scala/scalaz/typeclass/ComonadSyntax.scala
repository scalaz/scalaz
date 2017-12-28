package scalaz
package typeclass

import scala.language.experimental.macros

trait ComonadSyntax {
  implicit final class ToComonadOps[F[_]: Comonad, A](self: F[A]) {
    def copoint: A = macro meta.Ops.f_0
  }
}
