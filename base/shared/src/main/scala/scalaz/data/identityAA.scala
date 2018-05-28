package scalaz
package data

import scala.AnyVal

import scalaz.core.EqClass
import scalaz.ct.MonadClass

final case class Identity[A](run: A) extends AnyVal

trait IdentityInstances {

  implicit val monad: Monad[Identity] = instanceOf(new MonadClass[Identity] {
    override def map[A, B](fa: Identity[A])(f: A => B): Identity[B]               = Identity(f(fa.run))
    override def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B]      = Identity(f.run(fa.run))
    override def pure[A](a: A): Identity[A]                                       = Identity(a)
    override def flatMap[A, B](oa: Identity[A])(f: A => Identity[B]): Identity[B] = f(oa.run)
    override def flatten[A](ma: Identity[Identity[A]]): Identity[A]               = ma.run
  })

  implicit final def identityEq[A](implicit A: Eq[A]): Eq[Identity[A]] =
    instanceOf[EqClass[Identity[A]]](
      (x, y) =>
        (x, y) match {
          case (Identity(a1), Identity(a2)) => A.equal(a1, a2)
          case _                            => false
      }
    )
}
