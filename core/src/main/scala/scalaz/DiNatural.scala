package scalaz

trait DiNatural[F[_, _], G[_, _]] {
  def apply[A](f: F[A, A]): G[A, A]
}
