package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Coapplicative` */
final class CoapplicativeOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Coapplicative[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCoapplicativeOpsU[TC[F[_]] <: Coapplicative[F]] {
  implicit def ToCoapplicativeOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new CoapplicativeOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCoapplicativeOps0[TC[F[_]] <: Coapplicative[F]] extends ToCoapplicativeOpsU[TC] {
  implicit def ToCoapplicativeOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new CoapplicativeOps[F,A](v)

  ////

  ////
}

trait ToCoapplicativeOps[TC[F[_]] <: Coapplicative[F]] extends ToCoapplicativeOps0[TC] with ToCoapplicativeDecidableOps[TC]

trait CoapplicativeSyntax[F[_]] extends CoapplicativeDecidableSyntax[F] {
  implicit def ToCoapplicativeOps[A](v: F[A]): CoapplicativeOps[F, A] = new CoapplicativeOps[F,A](v)(CoapplicativeSyntax.this.F)

  def F: Coapplicative[F]
  ////

  ////
}
