package scalaz

trait MonadReaderLike[F[_,_],S] extends MonadLike[({type f[x]=F[S,x]})#f] {
  def ask: F[S,S]
  def local[A](f: S => S)(fa: F[S,A]): F[S,A]
  def scope[A](k: S)(fa: F[S,A]): F[S,A] = local(_ => k)(fa)
  def asks[A](f: S => A): F[S,A] = map(ask)(f)
}
trait MonadReader[F[_,_],S] extends MonadReaderLike[F,S]
trait MonadReaderInstance[F[_,_],S] extends MonadReader[F,S] with MonadInstance[({type f[x]=F[S,x]})#f]
