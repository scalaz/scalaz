package scalaz

////
/**
 *
 */
////
trait MonadError[F[_], E] extends Monad[F] { self =>
  ////

  def raiseError[A](e: E): F[A]
  def handleError[A](fa: F[A])(f: E => F[A]): F[A]

  trait MonadErrorLaw {
    def raisedErrorsHandled[A](e: E, f: E => F[A])(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(handleError(raiseError(e))(f), f(e))
    def errorsRaised[A](a: A, e: E)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(bind(point(a))(_ => raiseError(e)), raiseError(e))
    def errorsStopComputation[A](e: E, a: A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(bind(raiseError(e))(_ => point(a)), raiseError(e))
  }
  def monadErrorLaw = new MonadErrorLaw {}

  ////
  val monadErrorSyntax = new scalaz.syntax.MonadErrorSyntax[F, E] { def F = MonadError.this }
}

object MonadError {
  @inline def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F

  ////

  ////
}
