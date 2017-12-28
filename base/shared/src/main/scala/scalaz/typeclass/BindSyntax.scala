package scalaz
package typeclass

import scala.language.experimental.macros

trait BindSyntax {
  implicit final class ToBindOps[M[_]: Bind, A](ma: M[A]) {
    def flatMap[B](f: A => M[B]): M[B] = macro meta.Ops.f_1
  }
}

