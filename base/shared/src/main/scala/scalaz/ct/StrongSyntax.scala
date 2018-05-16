package scalaz
package ct

import scala.language.experimental.macros

trait StrongSyntax {
  implicit final class ToStrongOps[F[_, _], A, B](self: F[A, B]) {
    def first[C](implicit ev: Strong[F]): F[(A, C), (B, C)] = macro meta.Ops.i_0
    def second[C](implicit ev: Strong[F]): F[(C, A), (C, B)] = macro meta.Ops.i_0
  }
}
