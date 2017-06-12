package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait CobindSyntax {
  implicit def cobindOps[F[_], A](fa: F[A]): CobindSyntax.Ops[F, A] = new CobindSyntax.Ops(fa)
}

object CobindSyntax {
  class Ops[F[_], A](fa: F[A]) {
    def cobind[B](f: F[A] => B): F[B] = macro meta.Ops._f1[F[A] => B, F[B]]
  }
}
