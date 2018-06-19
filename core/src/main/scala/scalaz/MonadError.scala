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
  val monadErrorSyntax = new scalaz.syntax.MonadErrorSyntax[F, S] { def F = MonadError.this }
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
  /** The Free instruction set for MonadError */
  sealed abstract class Ast[E, A]
  final case class RaiseError[E, A](e: E) extends Ast[E, A]
  final case class HandleError[F[_], E, A](fa: F[A], f: E => F[A]) extends Ast[E, A]

  /** Extensible Effect */
  def liftF[F[_], E](
    implicit I: Ast[E, ?] :<: F
  ): MonadError[Free[F, ?], E] with BindRec[Free[F, ?]] =
    new MonadError[Free[F, ?], E] with BindRec[Free[F, ?]] {
      val delegate = Free.freeMonad[F]
      def point[A](a: =>A): Free[F, A] = delegate.point(a)
      def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]) = delegate.bind(fa)(f)
      override def map[A, B](fa: Free[F, A])(f: A => B) = delegate.map(fa)(f)
      override def tailrecM[A, B](f: A => Free[F, A \/ B])(a: A) = delegate.tailrecM(f)(a)

      def raiseError[A](e: E): Free[F, A] = Free.liftF(I.inj(RaiseError[E, A](e)))
      def handleError[A](fa: Free[F, A])(f: E => Free[F, A]): Free[F, A] = Free.liftF(I.inj(HandleError[Free[F, ?], E, A](fa, f)))
    }
  ////
}
