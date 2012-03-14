package scalaz

trait ComonadTrans[F[_[_], _]] {
  def lower[G[_]: CoBind, A](a: F[G, A]): G[A]
}

object ComonadTrans {
  def apply[F[_[_], _]](implicit F: ComonadTrans[F]): ComonadTrans[F] = F
}

trait CoHoist[F[_[_], _]] extends ComonadTrans[F] {
  def cohoist[M[_], N[_]: CoMonad](f: M ~> N): ({type f[x] = F[M, x]})#f ~> ({type f[x] = F[N, x]})#f
}
