package scalaz
package tc

import scala.List

import scala.language.experimental.macros

trait FoldableClass[F[_]] {

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

  def msuml[A: Monoid](fa: F[A]): A

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B // = TODO default implementation from foldmap

  // TODO Use IList (`toIList`)
  def toList[A](fa: F[A]): List[A]
}

object FoldableClass {

  trait DeriveToList[F[_]] extends FoldableClass[F] {
    final override def toList[A](fa: F[A]) = foldLeft(fa, List[A]())((t, h) => h :: t).reverse
  }

  trait DeriveFoldRight[F[_]] extends FoldableClass[F] with Alt[DeriveFoldRight[F]] {
    override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B // = TODO implement from foldmap/endo
  }

  trait DeriveFoldMap[F[_]] extends FoldableClass[F] with Alt[DeriveFoldMap[F]] {
    final override def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]) =
      foldRight(fa, B.mempty)((a, b) => B.mappend(f(a), b))

    final override def msuml[A](fa: F[A])(implicit A: Monoid[A]) =
      foldLeft(fa, A.mempty)((a, s) => A.mappend(a, s))
  }

  trait Alt[D <: Alt[D]]

}

trait FoldableSyntax {
  implicit final class ToFoldableOps[F[_], A](self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B)(implicit ev: Foldable[F]): B = macro meta.Ops.ia_1_1
    def foldRight[B](f: => B)(g: (A, => B) => B)(implicit ev: Foldable[F]): B = macro meta.Ops.ia_1_1
    def foldMap[B](f: A => B)(implicit g: Monoid[B], ev: Foldable[F]): B = macro meta.Ops.i_1_1i
    def msuml(implicit g: Monoid[A], ev: Foldable[F]): A = macro meta.Ops.i_0_1i
    def toList(implicit ev: Foldable[F]): List[A] = macro meta.Ops.i_0
  }
}
