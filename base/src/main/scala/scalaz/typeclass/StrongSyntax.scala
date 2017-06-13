package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait StrongSyntax {
  implicit def strongOps[F[_, _], A, B](fa: F[A, B])(implicit F: Strong[F]): StrongSyntax.Ops[F, A, B] =
    new StrongSyntax.Ops(fa)
}

object StrongSyntax {
  class Ops[F[_, _], A, B](self: F[A, B])(implicit F: Strong[F]) {
    def first[C]: F[(A, C), (B, C)] = macro meta.Ops._f0[F[(A, C), (B, C)]]
    def second[C]: F[(C, A), (C, B)] = macro meta.Ops._f0[F[(C, A), (C, B)]]
  }
}
