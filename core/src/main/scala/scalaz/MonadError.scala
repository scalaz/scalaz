package scalaz

trait BaseMonadError[F[_], S] extends Monad[F] {
  def raiseError[A](e: S): F[A]
  def handleError[A](fa: F[A])(f: S => F[A]): F[A]
}

////
/**
 *
 */
////
trait MonadError[F[_, _], S] extends BaseMonadError[F[S, ?], S] { self =>
  ////

  trait MonadErrorLaw {
    def raisedErrorsHandled[A](e: S, f: S => F[S, A])(implicit FEA: Equal[F[S, A]]): Boolean =
      FEA.equal(handleError(raiseError(e))(f), f(e))
    def errorsRaised[A](a: A, e: S)(implicit FEA: Equal[F[S, A]]): Boolean =
      FEA.equal(bind(point(a))(_ => raiseError(e)), raiseError(e))
    def errorsStopComputation[A](e: S, a: A)(implicit FEA: Equal[F[S, A]]): Boolean =
      FEA.equal(bind(raiseError(e))(_ => point(a)), raiseError(e))
  }
  def monadErrorLaw = new MonadErrorLaw {}

  ////
  val monadErrorSyntax = new scalaz.syntax.MonadErrorSyntax[F, S] { def F = MonadError.this }
}

object MonadError {
  @inline def apply[F[_, _], S](implicit F: MonadError[F, S]): MonadError[F, S] = F

  ////

  ////
}
