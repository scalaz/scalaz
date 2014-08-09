package scalaz
package syntax

final class MonadErrorIdOps[E](val self: E) extends AnyVal {
  def raiseError[F[_, _], A](implicit F: MonadError[F, E]): F[E, A] =
    F.raiseError[A](self)
}
