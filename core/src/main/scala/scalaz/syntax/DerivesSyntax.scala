package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Derives` */
final class DerivesOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Derives[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDerivesOpsU[TC[F[_]] <: Derives[F]] {
  implicit def ToDerivesOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new DerivesOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToDerivesOps0[TC[F[_]] <: Derives[F]] extends ToDerivesOpsU[TC] {
  implicit def ToDerivesOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new DerivesOps[F,A](v)

  ////

  ////
}

trait ToDerivesOps[TC[F[_]] <: Derives[F]] extends ToDerivesOps0[TC]

trait DerivesSyntax[F[_]]  {
  implicit def ToDerivesOps[A](v: F[A]): DerivesOps[F, A] = new DerivesOps[F,A](v)(DerivesSyntax.this.F)

  def F: Derives[F]
  ////

  ////
}
