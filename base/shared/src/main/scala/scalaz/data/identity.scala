package scalaz
package data

import kernel.instanceOf
import core.EqClass
import ct.MonadClass
import debug.DebugClass

sealed abstract class IdentityModule {
  type Identity[A]

  def apply[A](a: A): Identity[A]
  def run[A](id: Identity[A]): A

  def identityMonad: Monad[Identity]

  def identityEq[A: Eq]: Eq[Identity[A]]
  def identityDebug[A: Debug]: Debug[Identity[A]]
}

object IdentityModule {
  implicit def identityMonad: Monad[Identity] = Identity.identityMonad

  implicit def identityEq[A: Eq]: Eq[Identity[A]]          = Identity.identityEq
  implicit def identityDebug[A: Debug]: Debug[Identity[A]] = Identity.identityDebug
}

private[data] object IdentityImpl extends IdentityModule {
  type Identity[A] = A

  def apply[A](a: A): Identity[A] = a
  def run[A](id: Identity[A]): A  = id

  def identityMonad: Monad[Identity] =
    instanceOf(new MonadClass[Identity] {
      override def map[A, B](fa: Identity[A])(f: A => B): Identity[B]               = f(fa)
      override def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B]      = f(fa)
      override def pure[A](a: A): Identity[A]                                       = a
      override def flatMap[A, B](oa: Identity[A])(f: A => Identity[B]): Identity[B] = f(oa)
      override def flatten[A](ma: /*Identity[*/ Identity[A] /*]*/ ): Identity[A]    = ma
    })

  def identityEq[A](implicit A: Eq[A]): Eq[Identity[A]] =
    instanceOf(new EqClass[A] {
      def equal(first: A, second: A): Boolean = A.equal(first, second)
    })

  implicit final def identityDebug[A: Debug]: Debug[Identity[A]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Identity[A]](a => z"Identity($a)")
  }
}
