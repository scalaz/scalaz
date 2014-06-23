package scalaz

trait MonadError[F[_,_], E] extends Monad[({ type λ[α] = F[E, α] })#λ] {
  def raiseError[A](e: E): F[E, A]
  def handleError[A](fa: F[E, A])(f: E => F[E, A]): F[E, A]

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
