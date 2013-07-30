package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Contravariant` */
final class ContravariantOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Contravariant[F]) extends Ops[F[A]] {
  ////
  final def contramap[B](f: B => A): F[B] = F.contramap(self)(f)
  final def ∙[B](f: B => A): F[B] = F.contramap(self)(f)
  ////
}

sealed trait ToContravariantOps0 {
  implicit def ToContravariantOpsUnapply[FA](v: FA)(implicit F0: Unapply[Contravariant, FA]) =
    new ContravariantOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToContravariantOps extends ToContravariantOps0 with ToInvariantFunctorOps {
  implicit def ToContravariantOps[F[_],A](v: F[A])(implicit F0: Contravariant[F]) =
    new ContravariantOps[F,A](v)

  ////

  ////
}

trait ContravariantSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToContravariantOps[A](v: F[A]): ContravariantOps[F, A] = new ContravariantOps[F,A](v)(ContravariantSyntax.this.F)

  def F: Contravariant[F]
  ////

  ////
}
