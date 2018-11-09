package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cozip` */
final class CozipOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Cozip[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCozipOpsU[TC[F[_]] <: Cozip[F]] {
  implicit def ToCozipOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new CozipOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCozipOps0[TC[F[_]] <: Cozip[F]] extends ToCozipOpsU[TC] {
  implicit def ToCozipOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new CozipOps[F,A](v)

  ////

  ////
}

trait ToCozipOps[TC[F[_]] <: Cozip[F]] extends ToCozipOps0[TC]

trait CozipSyntax[F[_]]  {
  implicit def ToCozipOps[A](v: F[A]): CozipOps[F, A] = new CozipOps[F,A](v)(CozipSyntax.this.F)

  def F: Cozip[F]
  ////

  ////
}
