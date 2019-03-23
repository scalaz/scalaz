package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `InvariantAlt` */
final class InvariantAltOps[F[_],A] private[syntax](val self: F[A])(implicit val F: InvariantAlt[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToInvariantAltOpsU[TC[F[_]] <: InvariantAlt[F]] {
  implicit def ToInvariantAltOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new InvariantAltOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToInvariantAltOps0[TC[F[_]] <: InvariantAlt[F]] extends ToInvariantAltOpsU[TC] {
  implicit def ToInvariantAltOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new InvariantAltOps[F,A](v)

  ////

  ////
}

trait ToInvariantAltOps[TC[F[_]] <: InvariantAlt[F]] extends ToInvariantAltOps0[TC] with ToInvariantApplicativeOps[TC]

trait InvariantAltSyntax[F[_]] extends InvariantApplicativeSyntax[F] {
  implicit def ToInvariantAltOps[A](v: F[A]): InvariantAltOps[F, A] = new InvariantAltOps[F,A](v)(InvariantAltSyntax.this.F)

  def F: InvariantAlt[F]
  ////

  ////
}
