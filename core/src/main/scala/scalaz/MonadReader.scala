package scalaz

////
/**
 *
 */
////
trait MonadReader[F[_], S] extends Monad[F] { self =>
  ////

  def ask: F[S]
  def local[A](f: S => S)(fa: F[A]): F[A]
  def scope[A](k: S)(fa: F[A]): F[A] = local(_ => k)(fa)
  def asks[A](f: S => A): F[A] = map(ask)(f)

  ////

}

object MonadReader {
  @inline def apply[F[_], S](implicit F: MonadReader[F, S]): MonadReader[F, S] = F

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit A: MonadReader[G, E]): MonadReader[F, E] =
    new IsomorphismMonadReader[F, G, E] {
      override def G: MonadReader[G, E] = A
      override def iso: F <~> G = D
    }

  ////
  /** The Free instruction set for MonadReader */
  sealed abstract class Ast[S, A]
  final case class Ask[S]() extends Ast[S, S]
  final case class Local[F[_], S, A](f: S => S, fa: Free[F, A]) extends Ast[S, A]

  /** Extensible Effect */
  def liftF[F[_], S](
    implicit I: Ast[S, ?] :<: F
  ): MonadReader[Free[F, ?], S] with BindRec[Free[F, ?]] =
    new MonadReader[Free[F, ?], S] with BindRec[Free[F, ?]] {
      val delegate = Free.freeMonad[F]
      def point[A](a: =>A): Free[F, A] = delegate.point(a)
      def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]) = delegate.bind(fa)(f)
      override def map[A, B](fa: Free[F, A])(f: A => B) = delegate.map(fa)(f)
      override def tailrecM[A, B](f: A => Free[F, A \/ B])(a: A) = delegate.tailrecM(f)(a)

      def ask: Free[F, S] = Free.liftF(I.inj(Ask[S]()))
      def local[A](f: S => S)(fa: Free[F, A]): Free[F, A] = Free.liftF(I.inj(Local[F, S, A](f, fa)))
    }

  ////
}
