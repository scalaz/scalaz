package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
final class ComonadOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Comonad[F]) extends Ops[F[A]] {
  ////
  def copoint: A = F.copoint(self)

  ////
}

sealed trait ToComonadOpsU[TC[F[_]] <: Comonad[F]] {
  implicit def ToComonadOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): ComonadOps[F0.M, F0.A] =
    new ComonadOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToComonadOps0[TC[F[_]] <: Comonad[F]] extends ToComonadOpsU[TC] {
  implicit def ToComonadOps[F[_],A](v: F[A])(implicit F0: TC[F]): ComonadOps[F, A] =
    new ComonadOps[F, A](v)

  ////

  ////
}

trait ToComonadOps[TC[F[_]] <: Comonad[F]] extends ToComonadOps0[TC] with ToCobindOps[TC]

trait ComonadSyntax[F[_]] extends CobindSyntax[F] {
  implicit def ToComonadOps[A](v: F[A]): ComonadOps[F, A] = new ComonadOps[F,A](v)(ComonadSyntax.this.F)

  def F: Comonad[F]
  ////

  ////
}
