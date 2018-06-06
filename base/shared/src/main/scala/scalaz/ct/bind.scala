package scalaz
package ct

import scala.language.experimental.macros

@meta.minimal("flatMap", "flatten")
trait BindClass[M[_]] extends ApplyClass[M] {
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = flatten(map(ma)(f))
  def flatten[A](ma: M[M[A]]): M[A]               = flatMap(ma)(identity)

  override def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = flatMap(f)(map(fa))
}

trait BindFunctions {
  def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit M: Bind[M]): M[B] = M.flatMap(ma)(f)
}

trait BindInstances {}

trait BindSyntax {
  implicit final class ToBindOps[M[_], A](ma: M[A]) {
    def flatMap[B](f: A => M[B])(implicit ev: Bind[M]): M[B] = macro meta.Ops.i_1
  }
}
