package scalaz

////
/**
 *
 */
////
trait MonadError[F[_], S] extends Monad[F] { self =>
  ////

  def raiseError[A](e: S): F[A]
  def handleError[A](fa: F[A])(f: S => F[A]): F[A]

  trait MonadErrorLaw {
    def raisedErrorsHandled[A](e: S, f: S => F[A])(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(handleError(raiseError(e))(f), f(e))
    def errorsRaised[A](a: A, e: S)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(bind(point(a))(_ => raiseError(e)), raiseError(e))
    def errorsStopComputation[A](e: S, a: A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(bind(raiseError(e))(_ => point(a)), raiseError(e))
  }
  def monadErrorLaw = new MonadErrorLaw {}

  ////
  val monadErrorSyntax = new scalaz.syntax.MonadErrorSyntax[F, S] { def F = MonadError.this }
}

object MonadError {
  @inline def apply[F[_], S](implicit F: MonadError[F, S]): MonadError[F, S] = F

  ////
  import Isomorphism.<~>

  /**
   * Derives a MonadError for something isomorphic to a thing with a MonadError.
   */
  def fromIsoWithMonadError[F[_], G[_], E](
    D: F <~> G
  )(
    implicit ME: MonadError[G, E]
  ): MonadError[F, E] = new MonadError[F, E] {
    override def point[A](a: => A): F[A] =
      D.from(ME.point(a))
    override def raiseError[A](e: E): F[A] =
      D.from(ME.raiseError(e))
    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      D.from(ME.bind(D.to(fa))(a => D.to(f(a))))
    override def handleError[A](fa: F[A])(f: E => F[A]): F[A] =
      D.from(ME.handleError(D.to(fa))(a => D.to(f(a))))
  }

  ////
}
