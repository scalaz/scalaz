package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Applicative` */
trait ApplicativeOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Applicative[F]
  ////

  ////
}

trait ToApplicativeOps0 {
  implicit def ToApplicativeOpsUnapply[FA](v: FA)(implicit F0: Unapply[Applicative, FA]) =
    new ApplicativeOps[F0.M,F0.A] { def self = F0(v); implicit def F: Applicative[F0.M] = F0.TC }

}

trait ToApplicativeOps extends ToApplicativeOps0 with ToApplyOps with ToPointedOps {
  implicit def ToApplicativeOps[F[_],A](v: F[A])(implicit F0: Applicative[F]) =
    new ApplicativeOps[F,A] { def self = v; implicit def F: Applicative[F] = F0 }

  ////

  ////
}

trait ApplicativeSyntax[F[_]] extends ApplySyntax[F] with PointedSyntax[F] {
  implicit def ToApplicativeOps[A](v: F[A])(implicit F0: Applicative[F]): ApplicativeOps[F, A] = new ApplicativeOps[F,A] { def self = v; implicit def F: Applicative[F] = F0 }

  ////

  ////
}
