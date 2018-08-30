package scalaz
package syntax

import Leibniz.===

final class EitherTOps[V](val self: V) extends AnyVal {
  // to avoid a name collision with EitherOps, we suffix T to these methods
  def eitherT[F[_]: Applicative, A, B](implicit ev: V === (A \/ B)): EitherT[F, A, B] = EitherT.either(ev(self))
  def leftT[F[_]: Applicative, B]: EitherT[F, V, B] = EitherT.pureLeft(self)
  def rightT[F[_]: Applicative, A]: EitherT[F, A, V] = EitherT.pure(self)
}

trait ToEitherTOps {
  implicit def ToEitherTOps[A](a: A): EitherTOps[A] = new EitherTOps(a)
}
