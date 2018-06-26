package scalaz
package data

import scala.{ List, Nil }

import core.EqClass
import ct.{ BindClass, FoldableClass, MonadClass, TraversableClass }
import debug.DebugClass
import scalaz.algebra.MonoidClass

sealed abstract class IdentityModule {
  type Identity[A]

  def apply[A](a: A): Identity[A]
  def run[A](id: Identity[A]): A

  def monad: Monad[Identity]
  def traversable: Traversable[Identity]

  def monoid[A: Monoid]: Monoid[Identity[A]]
  def eq[A: Eq]: Eq[Identity[A]]
  def debug[A: Debug]: Debug[Identity[A]]
}

object IdentityModule {
  implicit def identityMonad: Monad[Identity]             = Identity.monad
  implicit def identityTraversable: Traversable[Identity] = Identity.traversable

  implicit def identityMonoid[A: Monoid]: Monoid[Identity[A]] = Identity.monoid
  implicit def identityEq[A: Eq]: Eq[Identity[A]]             = Identity.eq
  implicit def identityDebug[A: Debug]: Debug[Identity[A]]    = Identity.debug
}

private[data] object IdentityImpl extends IdentityModule {
  type Identity[A] = A

  def apply[A](a: A): Identity[A] = a
  def run[A](id: Identity[A]): A  = id

  def monad: Monad[Identity]             = instanceOf(instance)
  def traversable: Traversable[Identity] = instanceOf(instance)

  private val instance = new MonadClass[Identity] with BindClass.DeriveFlatten[Identity] with TraversableClass[Identity]
  with FoldableClass.DeriveFoldMap[Identity] {
    override def pure[A](a: A): Identity[A]                                                                      = a
    override def flatMap[A, B](ma: Identity[A])(f: A => Identity[B]): Identity[B]                                = f(ma)
    override def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B]                                     = f(fa)
    override def map[A, B](ma: Identity[A])(f: A => B): Identity[B]                                              = f(ma)
    override def traverse[F[_], A, B](ta: Identity[A])(f: A => F[B])(implicit F: Applicative[F]): F[Identity[B]] = f(ta)
    override def sequence[F[_], A](ta: Identity[F[A]])(implicit F: Applicative[F]): F[Identity[A]]               = ta
    override def foldRight[A, B](fa: Identity[A], z: => B)(f: (A, => B) => B): B                                 = f(fa, z)
    override def foldLeft[A, B](fa: Identity[A], z: B)(f: (B, A) => B): B                                        = f(z, fa)
    override def toList[A](fa: Identity[A]): List[A]                                                             = fa :: Nil
  }

  def monoid[A](implicit A: Monoid[A]): Monoid[Identity[A]] =
    instanceOf(new MonoidClass[A] {
      override def mempty: A                   = A.mempty
      override def mappend(a1: A, a2: => A): A = A.mappend(a1, a2)
    })

  def eq[A](implicit A: Eq[A]): Eq[Identity[A]] =
    instanceOf(new EqClass[A] {
      def equal(first: A, second: A): Boolean = A.equal(first, second)
    })

  implicit final def debug[A: Debug]: Debug[Identity[A]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Identity[A]](a => z"Identity($a)")
  }
}
