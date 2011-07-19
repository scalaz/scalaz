package scalaz

trait Distribute[F[_], G[_]] {
  def apply[A](f: F[G[A]]): G[F[A]]
}
