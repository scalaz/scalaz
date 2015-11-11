package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadError` */
final class MonadErrorOps[F[_], S, A] private[syntax](self: F[A])(implicit val F: MonadError[F, S]) {
  ////
  final def handleError(f: S => F[A]): F[A] =
    F.handleError(self)(f)

  ////
}

trait ToMonadErrorOps extends ToMonadOps {
  implicit def ToMonadErrorOps[F[_], S, A](v: F[A])(implicit F0: MonadError[F, S]) =
    new MonadErrorOps[F, S, A](v)

  ////

  implicit def ToMonadErrorIdOps[E](v: E) =
    new MonadErrorIdOps[E](v)

  ////
}

trait MonadErrorSyntax[F[_], S] extends MonadSyntax[F] {
  implicit def ToMonadErrorOps[A](v: F[A]): MonadErrorOps[F, S, A] =
    new MonadErrorOps[F, S, A](v)(MonadErrorSyntax.this.F)

  def F: MonadError[F, S]
  ////

  ////
}
