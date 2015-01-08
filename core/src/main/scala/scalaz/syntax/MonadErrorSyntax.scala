package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadError` */
final class MonadErrorOps[F[_, _], S, A] private[syntax](self: F[S, A])(implicit val F: MonadError[F, S]) {
  ////
  final def handleError(f: S => F[S, A]): F[S, A] =
    F.handleError(self)(f)

  ////
}

sealed trait ToMonadErrorOps0 {
  implicit def ToMonadErrorOpsUnapply[FA](v: FA)(implicit F0: Unapply21[MonadError, FA]) =
    new MonadErrorOps[F0.M, F0.A, F0.B](F0(v))(F0.TC)
}

trait ToMonadErrorOps extends ToMonadOps {
  implicit def ToMonadErrorOps[F[_, _], S, A](v: F[S, A])(implicit F0: MonadError[F, S]) =
    new MonadErrorOps[F, S, A](v)

  ////

 implicit def ToMonadErrorIdOps[E](v: E) =
   new MonadErrorIdOps[E](v)

  ////
}

trait MonadErrorSyntax[F[_, _], S] extends MonadSyntax[F[S, ?]] {
  implicit def ToMonadErrorOps[A](v: F[S, A]): MonadErrorOps[F, S, A] =
    new MonadErrorOps[F, S, A](v)(MonadErrorSyntax.this.F)

  def F: MonadError[F, S]
  ////

  ////
}
