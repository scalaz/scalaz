package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadError` */
final class MonadErrorOps[F[_], E, A] private[syntax](self: F[A])(implicit val F: MonadError[F, E]) {
  ////
  final def handleError(f: E => F[A]): F[A] =
    F.handleError(self)(f)

  ////
}

trait ToMonadErrorOps extends ToMonadOps {
  implicit def ToMonadErrorOps[F[_], E, A](v: F[A])(implicit F0: MonadError[F, E]) =
    new MonadErrorOps[F, E, A](v)

  ////

  implicit def ToMonadErrorIdOps[E](v: E) =
    new MonadErrorIdOps[E](v)

  ////
}

trait MonadErrorSyntax[F[_], E] extends MonadSyntax[F] {
  implicit def ToMonadErrorOps[A](v: F[A]): MonadErrorOps[F, E, A] =
    new MonadErrorOps[F, E, A](v)(MonadErrorSyntax.this.F)

  def F: MonadError[F, E]
  ////

  ////
}
