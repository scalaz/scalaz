package scalaz

// data-type
trait GeneralFunctor[C[_, _], D[_, _], F[_]] {
  def fmap[A, B](f: C[A, B]): D[F[A], F[B]]
}

object GeneralFunctor {
    
}
