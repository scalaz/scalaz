package scalaz
package data

import scala.{ inline, List, Nil }

import Predef._
import tc._

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
  @inline implicit def identityMonad: Monad[Identity]             = Identity.monad
  @inline implicit def identityTraversable: Traversable[Identity] = Identity.traversable

  @inline implicit def identityMonoid[A: Monoid]: Monoid[Identity[A]] = Identity.monoid
  @inline implicit def identityEq[A: Eq]: Eq[Identity[A]]             = Identity.eq
  @inline implicit def identityDebug[A: Debug]: Debug[Identity[A]]    = Identity.debug
}

private[data] object IdentityImpl extends IdentityModule {
  type Identity[A] = A

  def apply[A](a: A): Identity[A] = a
  def run[A](id: Identity[A]): A  = id

  val monad: Monad[Identity] =
    instanceOf(new MonadClass[Identity] {
      override def map[A, B](fa: Identity[A])(f: A => B): Identity[B]               = f(fa)
      override def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B]      = f(fa)
      override def pure[A](a: A): Identity[A]                                       = a
      override def flatMap[A, B](oa: Identity[A])(f: A => Identity[B]): Identity[B] = f(oa)
      override def flatten[A](ma: /*Identity[*/ Identity[A] /*]*/ ): Identity[A]    = ma
    })

  val traversable: Traversable[Identity] =
    instanceOf(new TraversableClass[Identity] {
      override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = f(fa)
      override def traverse[G[_]: Applicative, A, B](fa: Identity[A])(f: A => G[B]): G[Identity[B]] =
        f(fa)
      override def sequence[G[_]: Applicative, A](fa: Identity[G[A]]): G[Identity[A]] =
        fa
      override def foldLeft[A, B](fa: Identity[A], z: B)(f: (B, A) => B): B =
        f(z, fa)
      override def foldMap[A, B: Monoid](fa: Identity[A])(f: A => B): B =
        f(fa)
      override def msuml[A: Monoid](fa: Identity[A]): A =
        fa
      override def foldRight[A, B](fa: Identity[A], z: => B)(f: (A, => B) => B): B =
        f(fa, z)
      override def toList[A](fa: Identity[A]): List[A] =
        fa :: Nil
    })

  def monoid[A](implicit A: Monoid[A]): Monoid[Identity[A]] =
    A.asInstanceOf[Monoid[Identity[A]]]

  def eq[A](implicit A: Eq[A]): Eq[Identity[A]] =
    A.asInstanceOf[Eq[Identity[A]]]

  def debug[A: Debug]: Debug[Identity[A]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Identity[A]](a => z"Identity($a)")
  }
}
