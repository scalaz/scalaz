package scalaz
package typeclass

import scala.language.experimental.macros

trait BindSyntax {
  implicit final class ToBindOps[M[_], A](ma: M[A]) {
    def flatMap[B](f: A => M[B])(implicit ev: Bind[M]): M[B] = macro meta.Ops.i_1
  }
}
