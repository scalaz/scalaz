package scalaz
package syntax

trait ToMonadTransOps {
  implicit def MonadTransGFGA[F[_[_], _], G[_], A](gfga: G[F[G, A]]): MonadTransGFGA[F, G, A] = new MonadTransGFGA[F, G, A](gfga)
}

final class MonadTransGFGA[F[_[_], _], G[_], A](val self: G[F[G, A]]) extends AnyVal {
  def wrapEffect(implicit F: MonadTrans[F], G: Monad[G]): F[G, A] = F.apply[G].join(F.liftM(self))
}
