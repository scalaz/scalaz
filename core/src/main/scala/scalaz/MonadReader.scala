package scalaz

////
/**
 *
 */
////
trait MonadReader[F[_], S] {
  ////
  def monad: Monad[F]

  def ask: F[S]
  def local[A](f: S => S)(fa: F[A]): F[A]
  def scope[A](k: S)(fa: F[A]): F[A] = local(_ => k)(fa)
  def asks[A](f: S => A): F[A] = monad.map(ask)(f)

  ////

}

object MonadReader {
  @inline def apply[F[_], S](implicit F: MonadReader[F, S]): MonadReader[F, S] = F

  ////

  ////
}
