package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Codivide` */
final class CodivideOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Codivide[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCodivideOpsU[TC[F[_]] <: Codivide[F]] {
  implicit def ToCodivideOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new CodivideOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCodivideOps0[TC[F[_]] <: Codivide[F]] extends ToCodivideOpsU[TC] {
  implicit def ToCodivideOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new CodivideOps[F,A](v)

  ////

  ////
}

trait ToCodivideOps[TC[F[_]] <: Codivide[F]] extends ToCodivideOps0[TC] with ToCoapplicativeCodivideOps[TC]

trait CodivideSyntax[F[_]] extends CoapplicativeCodivideSyntax[F] {
  implicit def ToCodivideOps[A](v: F[A]): CodivideOps[F, A] = new CodivideOps[F,A](v)(CodivideSyntax.this.F)

  def F: Codivide[F]
  ////

  ////
}
