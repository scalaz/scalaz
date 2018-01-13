package scalaz
package data


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