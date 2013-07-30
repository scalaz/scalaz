package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monad` */
final class MonadOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Monad[F]) extends Ops[F[A]] {
  ////

  def liftM[G[_[_], _]](implicit G: MonadTrans[G]): G[F, A] = G.liftM(self)

  ////
}

sealed trait ToMonadOps0 {
  implicit def ToMonadOpsUnapply[FA](v: FA)(implicit F0: Unapply[Monad, FA]) =
    new MonadOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToMonadOps extends ToMonadOps0 with ToApplicativeOps with ToBindOps {
  implicit def ToMonadOps[F[_],A](v: F[A])(implicit F0: Monad[F]) =
    new MonadOps[F,A](v)

  ////

  ////
}

trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F] {
  implicit def ToMonadOps[A](v: F[A]): MonadOps[F, A] = new MonadOps[F,A](v)(MonadSyntax.this.F)

  def F: Monad[F]
  ////

  ////
}
