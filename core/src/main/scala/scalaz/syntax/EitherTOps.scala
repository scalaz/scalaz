package scalaz
package syntax

final class EitherTOps[A, B](val self: A \/ B) extends AnyVal {
  def liftEitherT[M[_]](implicit M0: Monad[M]): EitherT[M, A, B] =
    EitherT(M0.point(self))
}

trait ToEitherTOps {
  implicit def ToEitherTOps[A, B](a: A \/ B) = new EitherTOps(a)
}