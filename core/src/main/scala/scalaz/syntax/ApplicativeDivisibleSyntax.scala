package scalaz
package syntax

trait ApplicativeDivisibleSyntax[F[_]] extends ApplyDivideSyntax[F]

trait ToApplicativeDivisibleOps[TC[F[_]] <: ApplyDivide[F]] extends ToApplyDivideOps[TC]
