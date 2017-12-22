package scalaz
package syntax

import Leibniz.===

final class EitherTOps[V](private val self: V) extends AnyVal {
  // to avoid a name collision with EitherOps, we suffix T to these methods
  def eitherT[F[_]: Applicative, A, B](implicit ev: V === (A \/ B)): EitherT[F, A, B] = EitherT.either(ev(self))
  def leftT[F[_]: Applicative, B]: EitherT[F, V, B] = EitherT.left(self)
  def rightT[F[_]: Applicative, A]: EitherT[F, A, V] = EitherT.right(self)
}

trait ToEitherTOps {
  implicit def ToEitherTOps[A](a: A): EitherTOps[A] = new EitherTOps(a)
}
