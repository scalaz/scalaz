package scalaz
package data

import scalaz.typeclass._

import FoldableClass._
import TraversableClass._

trait ConstInstances {
  implicit def traverse[R]: Traversable[Const[R, ?]] = new TraversableClass[Const[R, ?]] with FoldRight[Const[R, ?]] with Traverse[Const[R, ?]] {
    def map[A, B](ma: Const[R, A])(f: A => B): Const[R, B] = ma.retag

    def traverse[F[_], A, B](ta: Const[R, A])(f: A => F[B])(implicit F: Applicative[F]): F[Const[R, B]] =
      F.pure(ta.retag)

    def foldLeft[A, B](fa: Const[R, A], z: B)(f: (B, A) => B): B = z

    def foldRight[A, B](fa: Const[R, A], z: => B)(f: (A, => B) => B): B = z

    override def toList[A](fa: Const[R, A]): List[A] = Nil
  }

  implicit def functor[R]: Functor[Const[R, ?]] = new Functor[Const[R, ?]] {
    def map[A, B](fa: Const[R, A])(f: A => B): Const[R, B] =
      fa.retag[B]
  }

  implicit def apply[R](implicit R: Semigroup[R]): Apply[Const[R, ?]] = new Apply[Const[R, ?]] {
    def functor: Functor[Const[R, ?]] = Const.functor[R]
    def ap[A, B](fa: Const[R, A])(f: Const[R, A => B]): Const[R, B] =
      Const(R.append(fa.getConst, f.getConst))
  }

  implicit def applicative[R](implicit R: Monoid[R]): Applicative[Const[R, ?]] = new Applicative[Const[R, ?]] {
    def apply: Apply[Const[R, ?]] = implicitly
    def pure[A](a: A): Const[R, A] = Const(R.empty)
  }

  implicit def semigroup[A, B](implicit A: Semigroup[A]): Semigroup[Const[A, B]] = new Semigroup[Const[A, B]] {
    def append(a1: Const[A, B], a2: => Const[A, B]): Const[A, B] =
      Const(A.append(a1.getConst, a2.getConst))
  }

  implicit def monoid[A, B](implicit A: Monoid[A]): Monoid[Const[A, B]] = new Monoid[Const[A, B]] {
    def semigroup: Semigroup[Const[A, B]] = implicitly
    def empty: Const[A, B] = Const(A.empty)
  }
}
