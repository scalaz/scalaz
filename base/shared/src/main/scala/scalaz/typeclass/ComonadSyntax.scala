package scalaz
package typeclass
import com.github.ghik.silencer.silent

import scala.language.implicitConversions
import scala.language.experimental.macros

trait ComonadSyntax {
  implicit def comonadOps[F[_], A](fa: F[A])(implicit F: Comonad[F]): ComonadSyntax.Ops[F, A] =
    new ComonadSyntax.Ops(fa)
}

object ComonadSyntax {
  class Ops[F[_]: Comonad, A](@silent self: F[A]) {
    def copoint: A = macro meta.Ops._f0[A]
  }
}
