package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Decidable` */
final class DecidableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Decidable[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDecidableOpsU[TC[F[_]] <: Decidable[F]] {
  implicit def ToDecidableOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new DecidableOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToDecidableOps0[TC[F[_]] <: Decidable[F]] extends ToDecidableOpsU[TC] {
  implicit def ToDecidableOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new DecidableOps[F,A](v)

  ////

  ////
}

trait ToDecidableOps[TC[F[_]] <: Decidable[F]] extends ToDecidableOps0[TC] with ToDivisibleOps[TC] with ToDerivesOps[TC]

trait DecidableSyntax[F[_]] extends DivisibleSyntax[F] with DerivesSyntax[F] {
  implicit def ToDecidableOps[A](v: F[A]): DecidableOps[F, A] = new DecidableOps[F,A](v)(DecidableSyntax.this.F)

  def F: Decidable[F]
  ////

  ////
}
