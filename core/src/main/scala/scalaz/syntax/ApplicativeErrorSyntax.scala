package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativeError` */
final class ApplicativeErrorOps[F[_], S, A] private[syntax](self: F[A])(implicit val F: ApplicativeError[F, S]) {
  ////
  final def handleError(f: S => F[A]): F[A] =
    F.handleError(self)(f)

  final def recover(f: S => A): F[A] =
    F.handleError(self)(s => F.point(f(s)))

  final def attempt: F[S \/ A] =
    F.handleError(F.map(self)(a => \/.right[S, A](a)))(e => F.point(-\/(e)))
  ////
}

trait ToApplicativeErrorOps0[TC[F[_], S] <: ApplicativeError[F, S]] {
  implicit def ToApplicativeErrorOps[F[_], S, A](v: F[A])(implicit F0: TC[F, S]) =
    new ApplicativeErrorOps[F, S, A](v)

  ////

  ////
}

trait ToApplicativeErrorOps[TC[F[_], S] <: ApplicativeError[F, S]] extends ToApplicativeErrorOps0[TC] with ToApplicativeOps[λ[F[_] => TC[F, S] forSome { type S }]]

trait ApplicativeErrorSyntax[F[_], S] extends ApplicativeSyntax[F] {
  implicit def ToApplicativeErrorOps[A](v: F[A]): ApplicativeErrorOps[F, S, A] =
    new ApplicativeErrorOps[F, S, A](v)(ApplicativeErrorSyntax.this.F)

  def F: ApplicativeError[F, S]
  ////

  ////
}
