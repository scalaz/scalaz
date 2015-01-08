package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadTell` */
final class MonadTellOps[F[_, _], S, A] private[syntax](self: F[S, A])(implicit val F: MonadTell[F, S]) {
  ////

  final def :++>(w: => S): F[S, A] = F.bind(self)(a => F.map(F.tell(w))(_ => a))

  final def :++>>(f: A => S): F[S, A] =
    F.bind(self)(a => F.map(F.tell(f(a)))(_ => a))

  ////
}

sealed trait ToMonadTellOps0 {
  implicit def ToMonadTellOpsUnapply[FA](v: FA)(implicit F0: Unapply21[MonadTell, FA]) =
    new MonadTellOps[F0.M, F0.A, F0.B](F0(v))(F0.TC)
}

trait ToMonadTellOps extends ToMonadOps {
  implicit def ToMonadTellOps[F[_, _], S, A](v: F[S, A])(implicit F0: MonadTell[F, S]) =
    new MonadTellOps[F, S, A](v)

  ////

  ////
}

trait MonadTellSyntax[F[_, _], S] extends MonadSyntax[F[S, ?]] {
  implicit def ToMonadTellOps[A](v: F[S, A]): MonadTellOps[F, S, A] =
    new MonadTellOps[F, S, A](v)(MonadTellSyntax.this.F)

  def F: MonadTell[F, S]
  ////

  ////
}
