package scalaz

trait CoMonadTrans[F[_[_], _]] {
  def lower[G[_]: CoBind, A](a: F[G, A]): G[A]
}

object CoMonadTrans {
  def apply[F[_[_], _]](implicit F: CoMonadTrans[F]): CoMonadTrans[F] = F
}

trait CoHoist[F[_[_], _]] extends CoMonadTrans[F] {
  def cohoist[M[_], N[_]: CoMonad](f: M ~> N): ({type f[x] = F[M, x]})#f ~> ({type f[x] = F[N, x]})#f
}
