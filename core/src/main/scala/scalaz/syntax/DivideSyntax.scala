package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Divide` */
final class DivideOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Divide[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDivideOpsU[TC[F[_]] <: Divide[F]] {
  implicit def ToDivideOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): DivideOps[F0.M, F0.A] =
    new DivideOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToDivideOps0[TC[F[_]] <: Divide[F]] extends ToDivideOpsU[TC] {
  implicit def ToDivideOps[F[_],A](v: F[A])(implicit F0: TC[F]): DivideOps[F, A] =
    new DivideOps[F, A](v)

  ////

  ////
}

trait ToDivideOps[TC[F[_]] <: Divide[F]] extends ToDivideOps0[TC] with ToContravariantOps[TC]

trait DivideSyntax[F[_]] extends ContravariantSyntax[F] {
  implicit def ToDivideOps[A](v: F[A]): DivideOps[F, A] = new DivideOps[F,A](v)(using DivideSyntax.this.F)

  def F: Divide[F]
  ////

  ////
}
