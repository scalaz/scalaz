package scalaz

trait BaseMonadError[F[_], E] extends Monad[F] {
  def raiseError[A](e: E): F[A]
  def handleError[A](fa: F[A])(f: E => F[A]): F[A]
}

trait MonadError[F[_,_], E] extends BaseMonadError[({ type λ[α] = F[E, α] })#λ, E] {

  trait MonadErrorLaw {
    def raisedErrorsHandled[A](e: E, f: E => F[E, A])(implicit FEA: Equal[F[E, A]]): Boolean =
      FEA.equal(handleError(raiseError(e))(f), f(e))
    def errorsRaised[A](a: A, e: E)(implicit FEA: Equal[F[E, A]]): Boolean =
      FEA.equal(bind(point(a))(_ => raiseError(e)), raiseError(e))
    def errorsStopComputation[A](e: E, a: A)(implicit FEA: Equal[F[E, A]]): Boolean =
      FEA.equal(bind(raiseError(e))(_ => point(a)), raiseError(e))
  }
  def monadErrorLaw = new MonadErrorLaw {}

  val monadErrorSyntax = new syntax.MonadErrorSyntax[F, E] {
    val F = MonadError.this
  }
}

object MonadError {
  def apply[F[_, _], E](implicit F: MonadError[F, E]) = F
}
