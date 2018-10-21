package scalaz
package tc

import scala.List
import scala.language.experimental.macros

trait FoldableClass[F[_]] {
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B

  def foldRight[A, B: Delay](fa: F[A], z: B)(f: (A, B) => B): B

  def foldRightStrict[A, B](fa: F[A], z: B)(f: (A, B) => B): B // TODO: default impl via foldRight and trampoline

  def foldMap[A, B: Delay](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldRight(fa, B.mempty)((a, b) => B.mappend(f(a), b))

  def foldMapStrict[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft(fa, B.mempty)((b, a) => B.mappend(b, f(a)))

  def msuml[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.mempty)(A.mappend)

  // TODO Use IList (`toIList`)
  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa, List[A]())((t, h) => h :: t).reverse
}

trait FoldableSyntax {
  implicit final class ToFoldableOps[F[_], A](self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B)(implicit ev: Foldable[F]): B = macro ops.Ops.ia_1_1
    def foldRight[B: Delay](f: B)(g: (A, B) => B)(implicit ev: Foldable[F]): B = macro ops.Ops.ia_1_1_1i
    def foldRightStrict[B](f: B)(g: (A, B) => B)(implicit ev: Foldable[F]): B = macro ops.Ops.ia_1_1
    def foldMap[B: Delay: Monoid](f: A => B)(implicit ev: Foldable[F]): B = macro ops.Ops.i_1_2i
    def foldMapStrict[B: Monoid](f: A => B)(implicit ev: Foldable[F]): B = macro ops.Ops.i_1_1i
    def msuml(implicit g: Monoid[A], ev: Foldable[F]): A = macro ops.Ops.i_0_1i
    def toList(implicit ev: Foldable[F]): List[A] = macro ops.Ops.i_0
  }
}
