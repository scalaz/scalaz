package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CovariantDerives` */
final class CovariantDerivesOps[F[_],A] private[syntax](val self: F[A])(implicit val F: CovariantDerives[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCovariantDerivesOpsU[TC[F[_]] <: CovariantDerives[F]] {
  implicit def ToCovariantDerivesOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new CovariantDerivesOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCovariantDerivesOps0[TC[F[_]] <: CovariantDerives[F]] extends ToCovariantDerivesOpsU[TC] {
  implicit def ToCovariantDerivesOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new CovariantDerivesOps[F,A](v)

  ////

  ////
}

trait ToCovariantDerivesOps[TC[F[_]] <: CovariantDerives[F]] extends ToCovariantDerivesOps0[TC] with ToDerivesOps[TC] with ToCoapplicativeOps[TC] with ToApplicativeOps[TC]

trait CovariantDerivesSyntax[F[_]] extends DerivesSyntax[F] with CoapplicativeSyntax[F] with ApplicativeSyntax[F] {
  implicit def ToCovariantDerivesOps[A](v: F[A]): CovariantDerivesOps[F, A] = new CovariantDerivesOps[F,A](v)(CovariantDerivesSyntax.this.F)

  def F: CovariantDerives[F]
  ////

  ////
}
