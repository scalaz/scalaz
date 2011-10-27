package scalaz

trait MonadTrans[F[_[_], _]] {
  def hoist[M[_], N[_]](f: M ~> N): ({type f[x] = F[M, x]})#f ~> ({type f[x] = F[N, x]})#f

  def liftM[G[_] : Monad, A](a: G[A]): F[G, A]
}

object MonadTrans {
  def apply[F[_[_], _]](implicit F: MonadTrans[F]): MonadTrans[F] = F
}
