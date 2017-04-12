package scalaz

////
////
trait BifunctorParent[F[_, _]] { self: Bifunctor[F] =>
  ////

  /** Bifunctors are covariant by nature */
  def widen[A, B, C >: A, D >: B](fab: F[A, B]): F[C, D] =
    bimap(fab)(identity[C], identity[D])

  ////
}
