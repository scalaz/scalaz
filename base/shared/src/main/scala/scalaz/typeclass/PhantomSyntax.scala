package scalaz
package typeclass

import scala.language.experimental.macros

trait PhantomSyntax {
  implicit final class ToPhantomOps[F[_], A](self: F[A]) {
    def pmap[B](implicit ev: Phantom[F]): F[B] = macro meta.Ops.i_0
  }
}
