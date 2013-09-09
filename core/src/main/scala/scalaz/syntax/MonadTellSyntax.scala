package scalaz
package syntax

final class MonadTellOps[F[_, _], W, A](self: F[W, A])(implicit MT: MonadTell[F, W]) {

  final def :++>(w: => W): F[W, A] = MT.bind(self)(a => MT.map(MT.tell(w))(_ => a))

  final def :++>>(f: A => W): F[W, A] =
     MT.bind(self)(a => MT.map(MT.tell(f(a)))(_ => a))
}

sealed trait ToMonadTellOps0 {
  implicit def ToMonadTellOpsUnapply[FA](v: FA)(implicit F0: Unapply21[MonadTell, FA]) = 
    new MonadTellOps[F0.M, F0.A, F0.B](F0(v))(F0.TC)
}

trait ToMonadTellOps extends ToMonadTellOps0 {
  implicit def ToMonadTellOps[F[_, _], A, W](v: F[W, A])(implicit F0: MonadTell[F, W]) = 
    new MonadTellOps[F, W, A](v)(F0)
}

trait MonadTellSyntax[F[_, _], W] {
  implicit def ToMonadTellOps[A](v: F[W, A])(implicit F0: MonadTell[F, W]) =
    new MonadTellOps[F, W, A](v)(F0)
}
