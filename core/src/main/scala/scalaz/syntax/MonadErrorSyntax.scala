package scalaz
package syntax

final class MonadErrorOps[F[_, _], E, A](self: F[E, A])(implicit F: MonadError[F, E]) {
  final def handleError(f: E => F[E, A]): F[E, A] =
    F.handleError(self)(f)
}

final class MonadErrorIdOps[E](self: E) {
  def raiseError[F[_, _], A](implicit F: MonadError[F, E]): F[E, A] =
    F.raiseError[A](self)
}

trait ToMonadErrorOps {
  implicit def ToMonadErrorOps[F[_, _], E, A](v: F[E, A])(implicit F: MonadError[F, E]) =
    new MonadErrorOps[F, E, A](v)(F)
  implicit def ToMonadErrorIdOps[E](v: E) =
    new MonadErrorIdOps[E](v)
}

trait MonadErrorSyntax[F[_, _], E] {
  val F: MonadError[F, E]
  implicit def ToMonadErrorOps[A](v: F[E, A]) =
    new MonadErrorOps[F, E, A](v)(F)
}
