package scalaz
package data

import scala.{ List, Nil }

import scalaz.algebra._
import scalaz.core.EqClass
import scalaz.ct._
import scalaz.debug.DebugClass

sealed abstract class ConstModule {
  type Const[A, B]

  def apply[A, B](a: A): Const[A, B]
  def run[A, B](const: Const[A, B]): A

  def constApply[A: Semigroup]: Apply[Const[A, ?]]
  def constApplicative[A: Monoid]: Applicative[Const[A, ?]]
  def constTraverse[A]: Traversable[Const[A, ?]]
  def constPhantom[A]: Phantom[Const[A, ?]]

  def constSemigroup[A: Semigroup, B]: Semigroup[Const[A, B]]
  def constMonoid[A: Monoid, B]: Monoid[Const[A, B]]
  def constEq[A: Eq, B]: Eq[Const[A, B]]
  def constDebug[A: Debug, B]: Debug[Const[A, B]]
}

object ConstModule {
  implicit def constApply[A: Semigroup]: Apply[Const[A, ?]]          = Const.constApply
  implicit def constApplicative[A: Monoid]: Applicative[Const[A, ?]] = Const.constApplicative
  implicit def constTraverse[A]: Traversable[Const[A, ?]]            = Const.constTraverse
  implicit def constPhantom[A]: Phantom[Const[A, ?]]                 = Const.constPhantom

  implicit def constSemigroup[A: Semigroup, B]: Semigroup[Const[A, B]] = Const.constSemigroup
  implicit def constMonoid[A: Monoid, B]: Monoid[Const[A, B]]          = Const.constMonoid
  implicit def constEq[A: Eq, B]: Eq[Const[A, B]]                      = Const.constEq
  implicit def constDebug[A: Debug, B]: Debug[Const[A, B]]             = Const.constDebug
}

private[data] object ConstImpl extends ConstModule {
  type Const[A, B] = A

  def apply[A, B](a: A): Const[A, B] = a

  def run[A, B](const: Const[A, B]): A = const

  def constTraverse[R]: Traversable[Const[R, ?]] =
    instanceOf(new TraversableClass[Const[R, ?]] with ConstPhantom[R] {

      override def traverse[F[_], A, B](ta: Const[R, A])(f: A => F[B])(implicit F: Applicative[F]): F[Const[R, B]] =
        F.pure(ta)

      override def foldLeft[A, B](fa: Const[R, A], z: B)(f: (B, A) => B): B = z

      override def foldRight[A, B](fa: Const[R, A], z: => B)(f: (A, => B) => B): B = z

      override def toList[A](fa: Const[R, A]): List[A] = Nil
    })

  private trait ConstPhantom[R] extends PhantomClass[Const[R, ?]] {
    override def pmap[A, B](ma: Const[R, A]): Const[R, B] = ma
  }

  private trait ConstApply[R] extends ApplyClass[Const[R, ?]] with ConstPhantom[R] {
    def R: SemigroupClass[R]

    final override def ap[A, B](fa: Const[R, A])(f: Const[R, A => B]): Const[R, B] =
      R.append(fa, f)
  }

  private trait ConstApplicative[R] extends ApplicativeClass[Const[R, ?]] with ConstApply[R] {
    override def R: MonoidClass[R]
    final override def pure[A](a: A): Const[R, A] = R.empty
  }

  private trait ConstSemigroup[A, B] extends SemigroupClass[Const[A, B]] {
    def A: SemigroupClass[A]
    final override def append(a1: Const[A, B], a2: => Const[A, B]): Const[A, B] =
      A.append(a1, a2)
  }

  private trait ConstMonoid[A, B] extends MonoidClass[Const[A, B]] with ConstSemigroup[A, B] {
    override def A: MonoidClass[A]
    final override def empty: Const[A, B] = A.empty
  }

  private trait ConstEq[A, B] extends EqClass[Const[A, B]] {
    def A: EqClass[A]
    final override def equal(x: Const[A, B], y: Const[A, B]): Boolean =
      A.equal(x, y)
  }

  def constApply[R: Semigroup]: Apply[Const[R, ?]] =
    instanceOf(new ConstApply[R] {
      override val R = implicitly
    })

  def constApplicative[R: Monoid]: Applicative[Const[R, ?]] =
    instanceOf(new ConstApplicative[R] {
      override val R = implicitly
    })

  def constSemigroup[A: Semigroup, B]: Semigroup[Const[A, B]] =
    instanceOf(new ConstSemigroup[A, B] {
      override val A = implicitly
    })

  def constMonoid[A: Monoid, B]: Monoid[Const[A, B]] =
    instanceOf(new ConstMonoid[A, B] {
      override val A = implicitly
    })

  def constDebug[A, B](implicit A: Debug[A]): Debug[Const[A, B]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Const[A, B]](a => z"Const($a)")
  }

  def constEq[A: Eq, B]: Eq[Const[A, B]] =
    instanceOf(new ConstEq[A, B] {
      override val A = implicitly
    })

  def constPhantom[A]: Phantom[Const[A, ?]] =
    instanceOf(new ConstPhantom[A] {})
}
