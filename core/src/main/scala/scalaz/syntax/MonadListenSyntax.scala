package scalaz
package syntax

trait MonadListenOps[F[_, _], W, A] extends Ops[F[W, A]] {
  implicit def ML: MonadListen[F, W]

  final def written: F[W, W] = ML.map(ML.listen(self)){ case (_, w) => w }

  final def listen: F[W, (A, W)] = ML.listen[A](self)
}

sealed trait ToMonadListenOps0 {
  implicit def ToMonadListenOpsUnapply[FA](v: FA)(implicit F0: Unapply21[MonadListen, FA]) =
    new MonadListenOps[F0.M, F0.A, F0.B]{ def self = F0(v); implicit def ML = F0.TC }
}

trait ToMonadListenOps extends ToMonadListenOps0 with ToMonadTellOps {
  implicit def ToMonadListenOps[F[_, _], A, W](v: F[W, A])(implicit F0: MonadListen[F, W]) = 
    new MonadListenOps[F, W, A]{ def self = v; implicit def ML = F0 }
}

trait MonadListenSyntax[F[_, _], W] extends MonadTellSyntax[F, W] {
  implicit def ToMonadListenOps[A](v: F[W, A])(implicit F0: MonadListen[F, W]) =
    new MonadListenOps[F, W, A]{ def self = v; implicit def ML = F0 }
}
