package scalaz
package data

import typeclass._

trait IdentityInstances {
  implicit val monad: Monad[Identity] = new MonadClass[Identity] {
    override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity(f(fa.run))
    override def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B] = Identity(f.run.apply(fa.run))
    override def pure[A](a: A): Identity[A] = Identity(a)
    override def flatMap[A, B](oa: Identity[A])(f: A => Identity[B]): Identity[B] = f(oa.run)
    override def flatten[A](ma: Identity[Identity[A]]): Identity[A] = ma.run
  }
}
