package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
final class PlusOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Plus[F]) extends Ops[F[A]] {
  ////

  final def <+>(other: => F[A]): F[A] = F.plus(self, other)

  ////
}

sealed trait ToPlusOpsU[TC[F[_]] <: Plus[F]] {
  implicit def ToPlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): PlusOps[F0.M, F0.A] =
    new PlusOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToPlusOps0[TC[F[_]] <: Plus[F]] extends ToPlusOpsU[TC] {
  implicit def ToPlusOps[F[_],A](v: F[A])(implicit F0: TC[F]): PlusOps[F, A] =
    new PlusOps[F, A](v)

  ////

  ////
}

trait ToPlusOps[TC[F[_]] <: Plus[F]] extends ToPlusOps0[TC]

trait PlusSyntax[F[_]]  {
  implicit def ToPlusOps[A](v: F[A]): PlusOps[F, A] = new PlusOps[F,A](v)(using PlusSyntax.this.F)

  def F: Plus[F]
  ////

  ////
}
