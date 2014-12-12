package scalaz
package syntax

final class EitherTOps[A, B](val self: A \/ B) extends AnyVal {
  def liftEitherT[F[_]](implicit A0: Applicative[F]): EitherT[F, A, B] =
    EitherT(A0.point(self))
}

trait ToEitherTOps {
  implicit def ToEitherTOps[A, B](a: A \/ B) = new EitherTOps(a)
}