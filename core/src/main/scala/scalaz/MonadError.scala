package scalaz

////
/**
 *
 */
////
trait MonadError[F[_], S] extends Monad[F] with MonadErrorParent[F, S] { self =>
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
  val monadErrorSyntax: scalaz.syntax.MonadErrorSyntax[F, S] =
    new scalaz.syntax.MonadErrorSyntax[F, S] { def F = MonadError.this }
}

object MonadError {
  @inline def apply[F[_], S](implicit F: MonadError[F, S]): MonadError[F, S] = F

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit A: MonadError[G, E]): MonadError[F, E] =
    new IsomorphismMonadError[F, G, E] {
      override def G: MonadError[G, E] = A
      override def iso: F <~> G = D
    }

  ////
  ////
}
