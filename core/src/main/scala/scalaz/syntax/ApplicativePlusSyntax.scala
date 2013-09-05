package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativePlus` */
final class ApplicativePlusOps[F[_],A] private[syntax](val self: F[A])(implicit val F: ApplicativePlus[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToApplicativePlusOps0 {
  implicit def ToApplicativePlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[ApplicativePlus, FA]) =
    new ApplicativePlusOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToApplicativePlusOps extends ToApplicativePlusOps0 with ToApplicativeOps with ToPlusEmptyOps {
  implicit def ToApplicativePlusOps[F[_],A](v: F[A])(implicit F0: ApplicativePlus[F]) =
    new ApplicativePlusOps[F,A](v)

  ////

  ////
}

trait ApplicativePlusSyntax[F[_]] extends ApplicativeSyntax[F] with PlusEmptySyntax[F] {
  implicit def ToApplicativePlusOps[A](v: F[A]): ApplicativePlusOps[F, A] = new ApplicativePlusOps[F,A](v)(ApplicativePlusSyntax.this.F)

  def F: ApplicativePlus[F]
  ////

  ////
}
