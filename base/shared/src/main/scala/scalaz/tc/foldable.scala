package scalaz
package tc

import scala.List

import scala.language.experimental.macros

trait FoldableClass[F[_]] {
  /**
  Applies a binary operator to a start value and all elements of this sequence, going left to right.
  Note: this will not terminate for infinite-sized collections. For Example, Stream.
  */
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
  /**
  Applies a binary operator to all elements of this list and a start value, going right to left.
  */
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
  /**
  It traverse the collection type A and does transform to B by using F function.
  */
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldRight(fa, B.mempty)((a, b) => B.mappend(f(a), b))

  /**
  It does addition by initializing with 0 with foldLeft and Identity value of particular monoid.
  */
  def msuml[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.mempty)(A.mappend(_, _))

  // TODO Use IList (`toIList`)
  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa, List[A]())((t, h) => h :: t).reverse
}

trait FoldableSyntax {
  implicit final class ToFoldableOps[F[_], A](self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B)(implicit ev: Foldable[F]): B = macro ops.Ops.ia_1_1
    def foldRight[B](f: => B)(g: (A, => B) => B)(implicit ev: Foldable[F]): B = macro ops.Ops.ia_1_1
    def foldMap[B](f: A => B)(implicit g: Monoid[B], ev: Foldable[F]): B = macro ops.Ops.i_1_1i
    def msuml(implicit g: Monoid[A], ev: Foldable[F]): A = macro ops.Ops.i_0_1i
    def toList(implicit ev: Foldable[F]): List[A] = macro ops.Ops.i_0
  }
}
