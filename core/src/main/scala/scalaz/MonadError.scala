package scalaz

////
/**
 *
 */
////
trait MonadError[F[_], S] extends Monad[F] with ApplicativeError[F, S] { self =>
  ////

  def emap[A, B](fa: F[A])(f: A => S \/ B): F[B] =
    bind(fa)(a => f(a).fold(raiseError(_), pure(_)))

  trait MonadErrorLaw extends ApplicativeErrorLaws {
    def errorsRaised[A](a: A, e: S)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(bind(point(a))(_ => raiseError(e)), raiseError(e))
    def errorsStopComputation[A](e: S, a: A)(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(bind(raiseError[A](e))(point(_)), raiseError[A](e))
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

trait IsomorphismMonadError[F[_], G[_], S] extends MonadError[F, S] with IsomorphismMonad[F, G] with IsomorphismApplicativeError[F, G, S]{
  implicit def G: MonadError[G, S]
  ////
  ////
}
