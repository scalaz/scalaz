package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Distributive` */
final class DistributiveOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Distributive[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDistributiveOpsU[TC[F[_]] <: Distributive[F]] {
  implicit def ToDistributiveOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): DistributiveOps[F0.M, F0.A] =
    new DistributiveOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToDistributiveOps0[TC[F[_]] <: Distributive[F]] extends ToDistributiveOpsU[TC] {
  implicit def ToDistributiveOps[F[_],A](v: F[A])(implicit F0: TC[F]): DistributiveOps[F, A] =
    new DistributiveOps[F, A](v)

  ////

  ////
}

trait ToDistributiveOps[TC[F[_]] <: Distributive[F]] extends ToDistributiveOps0[TC] with ToFunctorOps[TC]

trait DistributiveSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToDistributiveOps[A](v: F[A]): DistributiveOps[F, A] = new DistributiveOps[F,A](v)(using DistributiveSyntax.this.F)

  def F: Distributive[F]
  ////

  ////
}
