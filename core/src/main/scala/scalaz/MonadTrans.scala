package scalaz


trait MonadTrans[F[_[_], _]] {
  def lift[G[_] : Monad, A](a: G[A]): F[G, A]
}

object MonadTrans extends MonadTranss

trait MonadTranss {

}