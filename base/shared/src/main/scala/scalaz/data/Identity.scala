package scalaz
package data

import typeclass.MonadClass

trait IdentityModule {
    type Identity[A]

    def runIdentity[A](identity: Identity[A]): A

    def apply[A](a: A): Identity[A]

    def subst[A]: Identity[A] Is A

    implicit val monad: Monad[Identity]
}

private[data] object IdentityImpl extends IdentityModule {
    type Identity[A] = A

    def runIdentity[A](identity: A): A = identity

    def apply[A](a: A): A = a

    def subst[A]: A Is A = Is.refl[A]

    implicit val monad: Monad[Identity] = new MonadClass[Identity] {
        override def map[A, B](fa: A)(f: A => B): B = f(fa)
        override def ap[A, B](fa: A)(f: A => B): B = f(fa)
        override def pure[A](a: A): A = a
        override def flatMap[A, B](oa: A)(f: A => B): Identity[B] = f(oa)
        override def flatten[A](ma: A): A = ma
    }
}
