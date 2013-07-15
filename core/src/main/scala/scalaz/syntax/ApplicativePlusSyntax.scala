package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativePlus` */
sealed abstract class ApplicativePlusOps[F[_],A] extends Ops[F[A]] {
  implicit def F: ApplicativePlus[F]
  ////

  ////
}

trait ToApplicativePlusOps0 {
  implicit def ToApplicativePlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[ApplicativePlus, FA]) =
    new ApplicativePlusOps[F0.M,F0.A] { def self = F0(v); implicit def F: ApplicativePlus[F0.M] = F0.TC }

}

trait ToApplicativePlusOps extends ToApplicativePlusOps0 with ToApplicativeOps with ToPlusEmptyOps {
  implicit def ToApplicativePlusOps[F[_],A](v: F[A])(implicit F0: ApplicativePlus[F]) =
    new ApplicativePlusOps[F,A] { def self = v; implicit def F: ApplicativePlus[F] = F0 }

  ////

  ////
}

trait ApplicativePlusSyntax[F[_]] extends ApplicativeSyntax[F] with PlusEmptySyntax[F] {
  implicit def ToApplicativePlusOps[A](v: F[A]): ApplicativePlusOps[F, A] = new ApplicativePlusOps[F,A] { def self = v; implicit def F: ApplicativePlus[F] = ApplicativePlusSyntax.this.F }

  def F: ApplicativePlus[F]
  ////

  ////
}
