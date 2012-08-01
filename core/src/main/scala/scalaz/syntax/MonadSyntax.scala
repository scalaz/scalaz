package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monad` */
trait MonadOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Monad[F]
  ////
  
  def liftM[G[_[_], _]](implicit G: MonadTrans[G]): G[F, A] = G.liftM(self)

  final def replicateM[G[_]](n: Int)(implicit G: Traverse[G], P: Pointed[G], N: Monoid[G[F[A]]]): F[G[A]] =
    Monad.replicateM[F, G, A](self, n)

  final def replicateM_(n: Int): F[Unit] =
    Monad.replicateM_[F, A](self, n)

  ////
}

trait ToMonadOps0 {
  implicit def ToMonadOpsUnapply[FA](v: FA)(implicit F0: Unapply[Monad, FA]) =
    new MonadOps[F0.M,F0.A] { def self = F0(v); implicit def F: Monad[F0.M] = F0.TC }

}

trait ToMonadOps extends ToMonadOps0 with ToApplicativeOps with ToBindOps {
  implicit def ToMonadOps[F[_],A](v: F[A])(implicit F0: Monad[F]) =
    new MonadOps[F,A] { def self = v; implicit def F: Monad[F] = F0 }

  ////

  ////
}

trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F] {
  implicit def ToMonadOps[A](v: F[A])(implicit F0: Monad[F]): MonadOps[F, A] = new MonadOps[F,A] { def self = v; implicit def F: Monad[F] = F0 }

  ////

  ////
}
