package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Contravariant` */
final class ContravariantOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Contravariant[F]) extends Ops[F[A]] {
  ////
  final def contramap[B](f: B => A): F[B] = F.contramap(self)(f)
  final def ∙[B](f: B => A): F[B] = F.contramap(self)(f)
  ////
}

sealed trait ToContravariantOpsU[TC[F[_]] <: Contravariant[F]] {
  implicit def ToContravariantOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new ContravariantOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToContravariantOps0[TC[F[_]] <: Contravariant[F]] extends ToContravariantOpsU[TC] {
  implicit def ToContravariantOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new ContravariantOps[F,A](v)

  ////

  ////
}

trait ToContravariantOps[TC[F[_]] <: Contravariant[F]] extends ToContravariantOps0[TC] with ToInvariantFunctorOps[TC]

trait ContravariantSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToContravariantOps[A](v: F[A]): ContravariantOps[F, A] = new ContravariantOps[F,A](v)(ContravariantSyntax.this.F)

  def F: Contravariant[F]
  ////

  ////
}
