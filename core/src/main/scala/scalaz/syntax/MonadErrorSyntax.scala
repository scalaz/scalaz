package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadError` */
final class MonadErrorOps[F[_], S, A] private[syntax](self: F[A])(implicit val F: MonadError[F, S]) {
  ////
  final def handleError(f: S => F[A]): F[A] =
    F.handleError(self)(f)

  final def emap[B](f: A => S \/ B): F[B] =
    F.bind(self)(a => f(a).fold(F.raiseError(_), F.pure(_)))

  final def recover(f: S => A): F[A] =
    F.handleError(self)(s => F.point(f(s)))

  final def attempt: F[S \/ A] =
    F.handleError(F.map(self)(a => \/.right[S, A](a)))(e => F.point(-\/(e)))

  ////
}

trait ToMonadErrorOps extends ToMonadOps {
  implicit def ToMonadErrorOps[F[_], S, A](v: F[A])(implicit F0: MonadError[F, S]): MonadErrorOps[F, S, A] =
    new MonadErrorOps[F, S, A](v)

  ////

  implicit def ToMonadErrorIdOps[E](v: E): MonadErrorIdOps[E] =
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
