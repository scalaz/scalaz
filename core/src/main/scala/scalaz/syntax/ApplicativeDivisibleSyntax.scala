package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativeDivisible` */
final class ApplicativeDivisibleOps[F[_],A] private[syntax](val self: F[A])(implicit val F: ApplicativeDivisible[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToApplicativeDivisibleOpsU[TC[F[_]] <: ApplicativeDivisible[F]] {
  implicit def ToApplicativeDivisibleOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new ApplicativeDivisibleOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToApplicativeDivisibleOps0[TC[F[_]] <: ApplicativeDivisible[F]] extends ToApplicativeDivisibleOpsU[TC] {
  implicit def ToApplicativeDivisibleOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new ApplicativeDivisibleOps[F,A](v)

  ////

  ////
}

trait ToApplicativeDivisibleOps[TC[F[_]] <: ApplicativeDivisible[F]] extends ToApplicativeDivisibleOps0[TC] with ToApplyDivideOps[TC]

trait ApplicativeDivisibleSyntax[F[_]] extends ApplyDivideSyntax[F] {
  implicit def ToApplicativeDivisibleOps[A](v: F[A]): ApplicativeDivisibleOps[F, A] = new ApplicativeDivisibleOps[F,A](v)(ApplicativeDivisibleSyntax.this.F)

  def F: ApplicativeDivisible[F]
  ////

  ////
}
