package scalaz
package tc

import scala.List
import Predef._

import scala.language.experimental.macros

trait FoldableClass[F[_]] {
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldRight(fa, B.mempty)((a, b) => B.mappend(f(a), b))

  def msuml[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.mempty)(A.mappend(_, _))

  def traverse_[T[_], A, B](ta: F[A])(f: A => T[B])(implicit T: Applicative[T]): T[Unit] =
    foldLeft(ta, T.pure(()))((r, a) => T.apply2(f(a), r)((_, _) => ()))

  def sequence_[T[_]: Applicative, A](fta: F[T[A]]): T[Unit] =
    traverse_(fta)(identity)

  // TODO Use IList (`toIList`)
  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa, List[A]())((t, h) => h :: t).reverse
}

object FoldableClass {
  implicit val instanceList: Foldable[List] = instanceOf(instances.list.control)
}

trait FoldableSyntax {
  implicit final class ToFoldableOps[F[_], A](self: F[A]) {
    def foldLeft[B](f: B)(g: (B, A) => B)(implicit ev: Foldable[F]): B = macro ops.Ops.ia_1_1
    def foldRight[B](f: => B)(g: (A, => B) => B)(implicit ev: Foldable[F]): B = macro ops.Ops.ia_1_1
    def foldMap[B](f: A => B)(implicit g: Monoid[B], ev: Foldable[F]): B = macro ops.Ops.i_1_1i
    def msuml(implicit g: Monoid[A], ev: Foldable[F]): A = macro ops.Ops.i_0_1i
    def traverse_[T[_], B](f: A => T[B])(implicit g: Applicative[T], ev: Foldable[F]): T[Unit] = macro ops.Ops.i_1_1i
    def toList(implicit ev: Foldable[F]): List[A] = macro ops.Ops.i_0
  }

  implicit final class ToNestedFoldableOps[F[_], T[_], A](self: F[T[A]]) {
    def sequence_(implicit g: Applicative[T], ev: Foldable[F]): T[Unit] = macro ops.Ops.i_0_1i
  }
}
