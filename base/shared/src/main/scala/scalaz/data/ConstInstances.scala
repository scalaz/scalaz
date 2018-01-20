package scalaz
package data

import typeclass.{FoldableClass, TraversableClass}

import FoldableClass._
import TraversableClass._

trait ConstInstances {
  implicit def constTraverse[R]: Traversable[Const[R, ?]] = new TraversableClass[Const[R, ?]] with FoldRight[Const[R, ?]] with Traverse[Const[R, ?]] {
    def map[A, B](ma: Const[R, A])(f: A => B): Const[R, B] = ma.retag

    def traverse[F[_], A, B](ta: Const[R, A])(f: A => F[B])(implicit F: Applicative[F]): F[Const[R, B]] =
      F.pure(ta.retag)

    def foldLeft[A, B](fa: Const[R, A], z: B)(f: (B, A) => B): B = z

    def foldRight[A, B](fa: Const[R, A], z: => B)(f: (A, => B) => B): B = z

    override def toList[A](fa: Const[R, A]): List[A] = Nil
  }

  trait ConstFunctor[R] extends Functor[Const[R, ?]] {
    def map[A, B](fa: Const[R, A])(f: A => B): Const[R, B] =
      fa.retag[B]
  }

  trait ConstApply[R] extends Apply[Const[R, ?]] with ConstFunctor[R] {
    protected val RS: Semigroup[R]
    def functor: Functor[Const[R, ?]] with this.type = this
    def ap[A, B](fa: Const[R, A])(f: Const[R, A => B]): Const[R, B] =
      Const(RS.append(fa.getConst, f.getConst))
  }

  trait ConstApplicative[R] extends Applicative[Const[R, ?]] with ConstApply[R]  {
    protected val RM: Monoid[R]
    protected val RS: Semigroup[R] = RM.semigroup
    def apply: Apply[Const[R, ?]] with this.type = this
    def pure[A](a: A): Const[R, A] = Const(RM.empty)
  }

  trait ConstSemigroup[A, B] extends Semigroup[Const[A, B]] {
    protected val AS: Semigroup[A]
    def append(a1: Const[A, B], a2: => Const[A, B]): Const[A, B] =
      Const(AS.append(a1.getConst, a2.getConst))
  }

  trait ConstMonoid[A, B] extends Monoid[Const[A, B]] with ConstSemigroup[A, B] {
    protected val AM: Monoid[A]
    protected val AS = AM.semigroup
    def semigroup: Semigroup[Const[A, B]] with this.type = this
    def empty: Const[A, B] = Const(AM.empty)
  }

  implicit def constFunctor[R]: Functor[Const[R, ?]] = new ConstFunctor[R] {}

  implicit def constApply[R](implicit R: Semigroup[R]): Apply[Const[R, ?]] = new ConstApply[R] {
    protected val RS = R
  }

  implicit def constApplicative[R](implicit R: Monoid[R]): Applicative[Const[R, ?]] = new ConstApplicative[R] {
    protected val RM = R
  }

  implicit def constSemigroup[A, B](implicit A: Semigroup[A]): Semigroup[Const[A, B]] = new ConstSemigroup[A, B] {
    protected val AS = A
  }

  implicit def constMonoid[A, B](implicit A: Monoid[A]): Monoid[Const[A, B]] = new ConstMonoid[A, B] {
    protected val AM = A
  }

  implicit def constShow[A, B](implicit A: Show[A]): Show[Const[A, B]] =
    a => A.show(a.getConst)
}
