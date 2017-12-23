package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoapplicativeCodivide` */
final class CoapplicativeCodivideOps[F[_],A] private[syntax](val self: F[A])(implicit val F: CoapplicativeCodivide[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCoapplicativeCodivideOpsU[TC[F[_]] <: CoapplicativeCodivide[F]] {
  implicit def ToCoapplicativeCodivideOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new CoapplicativeCodivideOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCoapplicativeCodivideOps0[TC[F[_]] <: CoapplicativeCodivide[F]] extends ToCoapplicativeCodivideOpsU[TC] {
  implicit def ToCoapplicativeCodivideOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new CoapplicativeCodivideOps[F,A](v)

  ////

  ////
}

trait ToCoapplicativeCodivideOps[TC[F[_]] <: CoapplicativeCodivide[F]] extends ToCoapplicativeCodivideOps0[TC]

trait CoapplicativeCodivideSyntax[F[_]]  {
  implicit def ToCoapplicativeCodivideOps[A](v: F[A]): CoapplicativeCodivideOps[F, A] = new CoapplicativeCodivideOps[F,A](v)(CoapplicativeCodivideSyntax.this.F)

  def F: CoapplicativeCodivide[F]
  ////

  ////
}
