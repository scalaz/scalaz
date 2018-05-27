package scalaz
package ct

import scala.List

trait FoldableClass[F[_]] {

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

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
      foldRight(fa, B.empty)((a, b) => B.append(f(a), b))
  }

  trait Alt[D <: Alt[D]]

}
