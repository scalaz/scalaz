package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplyDivide` */
final class ApplyDivideOps[F[_],A] private[syntax](val self: F[A])(implicit val F: ApplyDivide[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToApplyDivideOpsU[TC[F[_]] <: ApplyDivide[F]] {
  implicit def ToApplyDivideOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new ApplyDivideOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToApplyDivideOps0[TC[F[_]] <: ApplyDivide[F]] extends ToApplyDivideOpsU[TC] {
  implicit def ToApplyDivideOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new ApplyDivideOps[F,A](v)

  ////

  ////
}

trait ToApplyDivideOps[TC[F[_]] <: ApplyDivide[F]] extends ToApplyDivideOps0[TC] with ToInvariantFunctorOps[TC]

trait ApplyDivideSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToApplyDivideOps[A](v: F[A]): ApplyDivideOps[F, A] = new ApplyDivideOps[F,A](v)(ApplyDivideSyntax.this.F)

  def F: ApplyDivide[F]
  ////

  ////
}
