package scalaz
package data

import scala.{ List, Nil }

import scalaz.algebra._
import scalaz.core.EqClass
import scalaz.ct._
import scalaz.debug.DebugClass

final case class Const[A, B](getConst: A) {
  def retag[C]: Const[A, C] = this.asInstanceOf[Const[A, C]]
}

object Const extends ConstInstances

trait ConstInstances {
  implicit def constTraverse[R]: Traversable[Const[R, ?]] =
    instanceOf(
      new TraversableClass.DeriveSequence[Const[R, ?]] with FoldableClass.DeriveFoldMap[Const[R, ?]]
      with ConstFunctor[R] {

        override def traverse[F[_], A, B](ta: Const[R, A])(f: A => F[B])(implicit F: Applicative[F]): F[Const[R, B]] =
          F.pure(ta.retag)

        override def foldLeft[A, B](fa: Const[R, A], z: B)(f: (B, A) => B): B = z

        override def foldRight[A, B](fa: Const[R, A], z: => B)(f: (A, => B) => B): B = z

        override def toList[A](fa: Const[R, A]): List[A] = Nil
      }
    )

  private trait ConstFunctor[R] extends FunctorClass[Const[R, ?]] {
    final override def map[A, B](fa: Const[R, A])(f: A => B): Const[R, B] =
      fa.retag[B]
  }

  private trait ConstApply[R] extends ApplyClass[Const[R, ?]] with ConstFunctor[R] {
    def R: SemigroupClass[R]
    final override def ap[A, B](fa: Const[R, A])(f: Const[R, A => B]): Const[R, B] =
      Const(R.append(fa.getConst, f.getConst))
  }

  private trait ConstApplicative[R] extends ApplicativeClass[Const[R, ?]] with ConstApply[R] {
    override def R: MonoidClass[R]
    final override def pure[A](a: A): Const[R, A] = Const(R.empty)
  }

  private trait ConstSemigroup[A, B] extends SemigroupClass[Const[A, B]] {
    def A: SemigroupClass[A]
    final override def append(a1: Const[A, B], a2: => Const[A, B]): Const[A, B] =
      Const(A.append(a1.getConst, a2.getConst))
  }

  private trait ConstMonoid[A, B] extends MonoidClass[Const[A, B]] with ConstSemigroup[A, B] {
    override def A: MonoidClass[A]
    final override def empty: Const[A, B] = Const(A.empty)
  }

  private trait ConstEq[A, B] extends EqClass[Const[A, B]] {
    def A: EqClass[A]
    final override def equal(x: Const[A, B], y: Const[A, B]): Boolean =
      A.equal(x.getConst, y.getConst)
  }

  implicit def constApply[R: Semigroup]: Apply[Const[R, ?]] =
    instanceOf(new ConstApply[R] {
      override val R = implicitly
    })

  implicit def constApplicative[R: Monoid]: Applicative[Const[R, ?]] =
    instanceOf(new ConstApplicative[R] {
      override val R = implicitly
    })

  implicit def constSemigroup[A: Semigroup, B]: Semigroup[Const[A, B]] =
    instanceOf(new ConstSemigroup[A, B] {
      override val A = implicitly
    })

  implicit def constMonoid[A: Monoid, B]: Monoid[Const[A, B]] =
    instanceOf(new ConstMonoid[A, B] {
      override val A = implicitly
    })

  implicit def constDebug[A, B](implicit A: DebugClass[A]): DebugClass[Const[A, B]] =
    instanceOf[DebugClass[Const[A, B]]](
      a => A.debug(a.getConst)
    )

  implicit def constEq[A: Eq, B]: Eq[Const[A, B]] =
    instanceOf(new ConstEq[A, B] {
      override val A = implicitly
    })
}
