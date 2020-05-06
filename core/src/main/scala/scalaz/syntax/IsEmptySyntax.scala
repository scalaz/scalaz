package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `IsEmpty` */
final class IsEmptyOps[F[_],A] private[syntax](val self: F[A])(implicit val F: IsEmpty[F]) extends Ops[F[A]] {
  ////

  def isEmpty: Boolean = F.isEmpty(self)

  ////
}

sealed trait ToIsEmptyOpsU[TC[F[_]] <: IsEmpty[F]] {
  implicit def ToIsEmptyOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): IsEmptyOps[F0.M, F0.A] =
    new IsEmptyOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToIsEmptyOps0[TC[F[_]] <: IsEmpty[F]] extends ToIsEmptyOpsU[TC] {
  implicit def ToIsEmptyOps[F[_],A](v: F[A])(implicit F0: TC[F]): IsEmptyOps[F, A] =
    new IsEmptyOps[F, A](v)

  ////

  ////
}

trait ToIsEmptyOps[TC[F[_]] <: IsEmpty[F]] extends ToIsEmptyOps0[TC] with ToPlusEmptyOps[TC]

trait IsEmptySyntax[F[_]] extends PlusEmptySyntax[F] {
  implicit def ToIsEmptyOps[A](v: F[A]): IsEmptyOps[F, A] = new IsEmptyOps[F,A](v)(IsEmptySyntax.this.F)

  def F: IsEmpty[F]
  ////

  ////
}
