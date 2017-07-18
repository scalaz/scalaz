package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros

trait BindSyntax {
  implicit def bindOps[M[_], A](ma: M[A])(implicit M: Bind[M]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)
}

object BindSyntax {
  class Ops[M[_], A](ma: M[A])(implicit M: Bind[M]) {
    def flatMap[B](f: A => M[B]): M[B] = macro meta.Ops._f1[A => M[B], M[B]]
  }
}

