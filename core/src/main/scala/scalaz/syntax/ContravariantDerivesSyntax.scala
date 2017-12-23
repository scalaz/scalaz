package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ContravariantDerives` */
final class ContravariantDerivesOps[F[_],A] private[syntax](val self: F[A])(implicit val F: ContravariantDerives[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToContravariantDerivesOpsU[TC[F[_]] <: ContravariantDerives[F]] {
  implicit def ToContravariantDerivesOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new ContravariantDerivesOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToContravariantDerivesOps0[TC[F[_]] <: ContravariantDerives[F]] extends ToContravariantDerivesOpsU[TC] {
  implicit def ToContravariantDerivesOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new ContravariantDerivesOps[F,A](v)

  ////

  ////
}

trait ToContravariantDerivesOps[TC[F[_]] <: ContravariantDerives[F]] extends ToContravariantDerivesOps0[TC] with ToDerivesOps[TC] with ToCodivideOps[TC] with ToDivisibleOps[TC]

trait ContravariantDerivesSyntax[F[_]] extends DerivesSyntax[F] with CodivideSyntax[F] with DivisibleSyntax[F] {
  implicit def ToContravariantDerivesOps[A](v: F[A]): ContravariantDerivesOps[F, A] = new ContravariantDerivesOps[F,A](v)(ContravariantDerivesSyntax.this.F)

  def F: ContravariantDerives[F]
  ////

  ////
}
