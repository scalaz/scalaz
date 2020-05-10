package scalaz
package syntax

final class MonadListenOps[F[_], W, A] private[syntax](self: F[A])(implicit ML: MonadListen[F, W]) {

  final def written: F[W] =
    ML.map(ML.listen(self)){ case (_, w) => w }

  final def listen: F[(A, W)] =
    ML.listen[A](self)
}

trait ToMonadListenOps0[TC[F[_], W] <: MonadListen[F, W]] {
  implicit def ToMonadListenOps[F[_], A, W](v: F[A])(implicit F0: TC[F, W]): MonadListenOps[F, W, A] =
    new MonadListenOps[F, W, A](v)(F0)
}

trait ToMonadListenOps[TC[F[_], W] <: MonadListen[F, W]] extends ToMonadListenOps0[TC] with ToMonadTellOps[TC]

trait MonadListenSyntax[F[_], W] extends MonadTellSyntax[F, W] {
  implicit def ToMonadListenOps[A](v: F[A])(implicit F0: MonadListen[F, W]): MonadListenOps[F, W, A] =
    new MonadListenOps[F, W, A](v)(F0)
}
