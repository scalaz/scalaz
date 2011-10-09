package scalaz

trait Arr[F[_, _]] {
  def arr[A, B](f: A => B): F[A, B]
}

// TODO CoKleisliArr instance