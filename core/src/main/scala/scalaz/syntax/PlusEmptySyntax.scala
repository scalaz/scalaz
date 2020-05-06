package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `PlusEmpty` */
final class PlusEmptyOps[F[_],A] private[syntax](val self: F[A])(implicit val F: PlusEmpty[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToPlusEmptyOpsU[TC[F[_]] <: PlusEmpty[F]] {
  implicit def ToPlusEmptyOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): PlusEmptyOps[F0.M, F0.A] =
    new PlusEmptyOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToPlusEmptyOps0[TC[F[_]] <: PlusEmpty[F]] extends ToPlusEmptyOpsU[TC] {
  implicit def ToPlusEmptyOps[F[_],A](v: F[A])(implicit F0: TC[F]): PlusEmptyOps[F, A] =
    new PlusEmptyOps[F, A](v)

  ////

  def mempty[F[_], A](implicit F: PlusEmpty[F]): F[A] = F.empty[A]
  ////
}

trait ToPlusEmptyOps[TC[F[_]] <: PlusEmpty[F]] extends ToPlusEmptyOps0[TC] with ToPlusOps[TC]

trait PlusEmptySyntax[F[_]] extends PlusSyntax[F] {
  implicit def ToPlusEmptyOps[A](v: F[A]): PlusEmptyOps[F, A] = new PlusEmptyOps[F,A](v)(PlusEmptySyntax.this.F)

  def F: PlusEmpty[F]
  ////

  ////
}
