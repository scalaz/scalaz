package scalaz
package typeclass

import scala.language.experimental.macros

trait StrongSyntax {
  implicit final class ToStrongOps[F[_, _]: Strong, A, B](self: F[A, B]) {
    def first[C]: F[(A, C), (B, C)] = macro meta.Ops.f_0
    def second[C]: F[(C, A), (C, B)] = macro meta.Ops.f_0
  }
}
