package scalaz
package typeclass

import com.github.ghik.silencer.silent

import scala.language.implicitConversions
import scala.language.experimental.macros

trait PhantomSyntax {
  implicit def phantomOps[F[_], A](fa: F[A])(implicit F: Phantom[F]): PhantomSyntax.Ops[F, A] =
    new PhantomSyntax.Ops(fa)
}

object PhantomSyntax {
  class Ops[F[_]: Phantom, A](@silent self: F[A]) {
    def pmap[B]: F[B] = macro meta.Ops._f0[F[B]]
  }
}
