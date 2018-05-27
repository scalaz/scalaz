package scalaz
package types

trait AsSyntax {
  implicit final class ToAsOps[A, B](val ab: As[A, B]) {
    def liftCvF[F[_]](implicit F: IsCovariant[F]): F[A] As F[B] =
      F.liftLiskov(ab)

    def liftCtF[F[_]](implicit F: IsContravariant[F]): F[B] As F[A] =
      F.liftLiskov(ab)

    def substCvF[F[_]](fa: F[A])(implicit F: IsCovariant[F]): F[B] = {
      type f[+x] = x
      F.substCv[f, A, B](fa)(ab)
    }

    def substCtF[F[_]](fb: F[B])(implicit F: IsContravariant[F]): F[A] = {
      type f[+x] = x
      F.substCv[f, A, B](fb)(ab)
    }
  }
}
