package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativePlus` */
trait ApplicativePlusV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: ApplicativePlus[F]
  ////

  ////
}

trait ToApplicativePlusV0 {
  implicit def ToApplicativePlusVUnapply[FA](v: FA)(implicit F0: Unapply[ApplicativePlus, FA]) =
    new ApplicativePlusV[F0.M,F0.A] { def self = F0(v); implicit def F: ApplicativePlus[F0.M] = F0.TC }

}

trait ToApplicativePlusV extends ToApplicativePlusV0 with ToApplicativeV with ToEmptyV {
  implicit def ToApplicativePlusV[F[_],A](v: F[A])(implicit F0: ApplicativePlus[F]) =
    new ApplicativePlusV[F,A] { def self = v; implicit def F: ApplicativePlus[F] = F0 }

  ////

  ////
}

trait ApplicativePlusSyntax[F[_]] extends ApplicativeSyntax[F] with EmptySyntax[F] {
  implicit def ToApplicativePlusV[A](v: F[A])(implicit F0: ApplicativePlus[F]): ApplicativePlusV[F, A] = new ApplicativePlusV[F,A] { def self = v; implicit def F: ApplicativePlus[F] = F0 }

  ////

  ////
}
