package scalaz
package clazz

import scala.language.implicitConversions

trait BindSyntax {
  def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit M: Bind[M]): M[B] = M.flatMap(ma)(f)

  implicit def bindOps[M[_], A](ma: M[A])(implicit M: Bind[M]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)
}

object BindSyntax {
  class Ops[M[_], A](ma: M[A])(implicit M: Bind[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.flatMap(ma)(f)
  }
}

