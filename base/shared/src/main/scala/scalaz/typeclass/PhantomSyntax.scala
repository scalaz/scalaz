package scalaz
package typeclass

import scala.language.experimental.macros

trait PhantomSyntax {
  implicit final class ToPhantomOps[F[_]: Phantom, A](self: F[A]) {
    def pmap[B]: F[B] = macro meta.Ops.f_0
  }
}
