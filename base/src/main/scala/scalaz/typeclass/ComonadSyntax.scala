package scalaz
package typeclass
import scala.language.implicitConversions
import scala.language.experimental.macros

trait ComonadSyntax {
  implicit def comonadOps[F[_], A](fa: F[A])(implicit F: Comonad[F]): ComonadSyntax.Ops[F, A] =
    new ComonadSyntax.Ops(fa)
}

object ComonadSyntax {
  class Ops[F[_], A](self: F[A])(implicit F: Comonad[F]) {
    def copoint: A = macro meta.Ops._f0[A]
  }
}
