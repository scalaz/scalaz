package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoapplicativeDecidable` */
final class CoapplicativeDecidableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: CoapplicativeDecidable[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCoapplicativeDecidableOpsU[TC[F[_]] <: CoapplicativeDecidable[F]] {
  implicit def ToCoapplicativeDecidableOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new CoapplicativeDecidableOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCoapplicativeDecidableOps0[TC[F[_]] <: CoapplicativeDecidable[F]] extends ToCoapplicativeDecidableOpsU[TC] {
  implicit def ToCoapplicativeDecidableOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new CoapplicativeDecidableOps[F,A](v)

  ////

  ////
}

trait ToCoapplicativeDecidableOps[TC[F[_]] <: CoapplicativeDecidable[F]] extends ToCoapplicativeDecidableOps0[TC]

trait CoapplicativeDecidableSyntax[F[_]]  {
  implicit def ToCoapplicativeDecidableOps[A](v: F[A]): CoapplicativeDecidableOps[F, A] = new CoapplicativeDecidableOps[F,A](v)(CoapplicativeDecidableSyntax.this.F)

  def F: CoapplicativeDecidable[F]
  ////

  ////
}
