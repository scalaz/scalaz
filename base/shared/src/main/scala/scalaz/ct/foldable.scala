package scalaz
package ct

import scala.{ List, Nil }

import scala.language.experimental.macros

trait FoldableClass[F[_]] {

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B =
    foldRight(fa, Monoid[B].empty)((a, b) => Monoid[B].append(f(a), b))

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B // = TODO implement from foldmap/endo

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B // = TODO default implementation from foldmap

  // TODO Use IList (`toIList`)
  def toList[A](fa: F[A]): List[A] = foldRight[A, List[A]](fa, Nil)(_ :: _)
}

trait FoldableInstances {}

trait FoldableSyntax {
  implicit final class ToFoldableOps[F[_], A](self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B)(implicit ev: Foldable[F]): B = macro meta.Ops.ia_1_1
    def foldRight[B](f: => B)(g: (A, => B) => B)(implicit ev: Foldable[F]): B = macro meta.Ops.ia_1_1
    def foldMap[B](f: A => B)(implicit g: Monoid[B], ev: Foldable[F]): B = macro meta.Ops.i_1_1i
    def toList(implicit ev: Foldable[F]): List[A] = macro meta.Ops.i_0
  }
}
