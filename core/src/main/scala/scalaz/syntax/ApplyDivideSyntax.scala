package scalaz
package syntax

trait ApplyDivideSyntax[F[_]] extends InvariantFunctorSyntax[F]

trait ToApplyDivideOps[TC[F[_]] <: ApplyDivide[F]] extends ToInvariantFunctorOps[TC]
