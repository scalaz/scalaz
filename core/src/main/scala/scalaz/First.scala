package scalaz

trait First[F[_, _]] {
  ////
  def first[A, B, C](f: F[A, B]): F[(A, C), (B, C)]

  ////
}

object First {
  ////

  ////
}