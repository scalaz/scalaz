package scalaz

trait MonadWriter[F[_, _], W] extends Monad[({type f[x] = F[W, x]})#f] {
  implicit def W: Monoid[W]
  
  def writer[A](v: (W, A)): F[W, A] = bind(tell(v._1))(_ => point(v._2))
  def tell(w: W): F[W, Unit] = writer((w, ()))
  def listen[A](fa: F[W, A]): F[W, (A, W)]
  def pass[A](fa: F[W, (A, W => W)]): F[W, A]
}

object MonadWriter {
  def apply[F[_, _], W](implicit F: MonadWriter[F, W]) = F
}