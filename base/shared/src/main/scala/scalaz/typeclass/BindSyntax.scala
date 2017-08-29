package scalaz
package typeclass

import com.github.ghik.silencer.silent

import scala.language.implicitConversions
import scala.language.experimental.macros

trait BindSyntax {
  implicit def bindOps[M[_], A](ma: M[A])(implicit M: Bind[M]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)
}

object BindSyntax {
  class Ops[M[_]: Bind, A](@silent ma: M[A]) {
    def flatMap[B](f: A => M[B]): M[B] = macro meta.Ops._f1[A => M[B], M[B]]
  }
}

