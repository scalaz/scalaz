package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Divide` */
final class DivideOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Divide[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDivideOps0 {
  implicit def ToDivideOpsUnapply[FA](v: FA)(implicit F0: Unapply[Divide, FA]): DivideOps[F0.M, F0.A] =
    new DivideOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToDivideOps extends ToDivideOps0 with ToContravariantOps {
  implicit def ToDivideOps[F[_], A](v: F[A])(implicit F0: Divide[F]): DivideOps[F, A] =
    new DivideOps[F, A](v)

  ////

  ////
}

trait DivideSyntax[F[_]] extends ContravariantSyntax[F] {
  implicit def ToDivideOps[A](v: F[A]): DivideOps[F, A] = new DivideOps[F,A](v)(DivideSyntax.this.F)

  def F: Divide[F]
  ////

  ////
}
