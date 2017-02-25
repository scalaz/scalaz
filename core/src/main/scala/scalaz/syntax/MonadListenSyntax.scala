package scalaz
package syntax

final class MonadListenOps[F[_], W, A] private[syntax](self: F[A])(implicit ML: MonadListen[F, W]) {

  final def written: F[W] =
    ML.map(ML.listen(self)){ case (_, w) => w }

  final def listen: F[(A, W)] =
    ML.listen[A](self)
}

trait ToMonadListenOps extends ToMonadTellOps {
  implicit def ToMonadListenOps[F[_], A, W](v: F[A])(implicit F0: MonadListen[F, W]) =
    new MonadListenOps[F, W, A](v)(F0)
}

trait MonadListenSyntax[F[_], W] extends MonadTellSyntax[F, W] {
  implicit def ToMonadListenOps[A](v: F[A])(implicit F0: MonadListen[F, W]) =
    new MonadListenOps[F, W, A](v)(F0)
}
