package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `IsEmpty` */
final class IsEmptyOps[F[_],A] private[syntax](val self: F[A])(implicit val F: IsEmpty[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToIsEmptyOps0 {
  implicit def ToIsEmptyOpsUnapply[FA](v: FA)(implicit F0: Unapply[IsEmpty, FA]) =
    new IsEmptyOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToIsEmptyOps extends ToIsEmptyOps0 with ToPlusEmptyOps {
  implicit def ToIsEmptyOps[F[_],A](v: F[A])(implicit F0: IsEmpty[F]) =
    new IsEmptyOps[F,A](v)

  ////

  ////
}

trait IsEmptySyntax[F[_]] extends PlusEmptySyntax[F] {
  implicit def ToIsEmptyOps[A](v: F[A]): IsEmptyOps[F, A] = new IsEmptyOps[F,A](v)(IsEmptySyntax.this.F)

  def F: IsEmpty[F]
  ////

  ////
}
