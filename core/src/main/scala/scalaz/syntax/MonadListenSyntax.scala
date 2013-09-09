package scalaz
package syntax

final class MonadListenOps[F[_, _], W, A] private[syntax](self: F[W, A])(implicit ML: MonadListen[F, W]) {

  final def written: F[W, W] = ML.map(ML.listen(self)){ case (_, w) => w }

  final def listen: F[W, (A, W)] = ML.listen[A](self)
}

sealed trait ToMonadListenOps0 {
  implicit def ToMonadListenOpsUnapply[FA](v: FA)(implicit F0: Unapply21[MonadListen, FA]) =
    new MonadListenOps[F0.M, F0.A, F0.B](F0(v))(F0.TC)
}

trait ToMonadListenOps extends ToMonadListenOps0 with ToMonadTellOps {
  implicit def ToMonadListenOps[F[_, _], A, W](v: F[W, A])(implicit F0: MonadListen[F, W]) = 
    new MonadListenOps[F, W, A](v)(F0)
}

trait MonadListenSyntax[F[_, _], W] extends MonadTellSyntax[F, W] {
  implicit def ToMonadListenOps[A](v: F[W, A])(implicit F0: MonadListen[F, W]) =
    new MonadListenOps[F, W, A](v)(F0)
}
