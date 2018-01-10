package scalaz
package data

import scalaz.typeclass.MonadClass


/**
  * The following laws should hold:
  *   abs :: Monad m => Codensity m a -> m a
  *   rep :: Monad m => m a -> Codensity m a
  *
  *   abs . rep = id_m && rep . abs = id_{Codensity_m}
  *
  */
trait CodensityModule {
  type Codensity[M[_], A]

  def abs[M[_], A](c: Codensity[M, A])(implicit M: Monad[M]): M[A]
  def rep[M[_], A](ma: M[A])(implicit M: Monad[M]): Codensity[M, A]
}

private[data] object CodensityImpl extends CodensityModule with CodensityInstances {
  type Codensity[M[_], A] = ∀[λ[β => (A => M[β]) => M[β]]]

  def abs[M[_], A](c: Codensity[M, A])(implicit M: Monad[M]): M[A] =
    ∀.specialize[λ[β => (A => M[β]) => M[β]], A](c).apply(M.applicative.pure)

  def rep[M[_], A](ma: M[A])(implicit M: Monad[M]): Codensity[M, A] =
    ∀.mk[∀[λ[β => (A => M[β]) => M[β]]]].from(M.bind.flatMap(ma))
}

trait CodensityInstances {
  implicit def monad[M[_]](implicit M: Monad[M]): MonadClass[Codensity[M, ?]] = new MonadClass.Template[Codensity[M, ?]] {
    override def pure[A](a: A): Codensity[M, A] = Codensity.rep(M.applicative.pure(a))
    override def ap[A, B](fa: Codensity[M, A])(f: Codensity[M, A => B]): Codensity[M, B] = flatMap(f)(map(fa))
    override def flatMap[A, B](ma: Codensity[M, A])(f: A => Codensity[M, B]): Codensity[M, B] =
      Codensity.rep(M.bind.flatMap(Codensity.abs(ma))(a => Codensity.abs(f(a))))
  }
}