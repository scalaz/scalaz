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
  ////
}
