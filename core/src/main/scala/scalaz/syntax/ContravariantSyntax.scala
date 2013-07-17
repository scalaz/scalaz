package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Contravariant` */
sealed abstract class ContravariantOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Contravariant[F]
  ////
  final def contramap[B](f: B => A): F[B] = F.contramap(self)(f)
  final def ∙[B](f: B => A): F[B] = F.contramap(self)(f)
  ////
}

trait ToContravariantOps0 {
  implicit def ToContravariantOpsUnapply[FA](v: FA)(implicit F0: Unapply[Contravariant, FA]) =
    new ContravariantOps[F0.M,F0.A] { def self = F0(v); implicit def F: Contravariant[F0.M] = F0.TC }

}

trait ToContravariantOps extends ToContravariantOps0 with ToInvariantFunctorOps {
  implicit def ToContravariantOps[F[_],A](v: F[A])(implicit F0: Contravariant[F]) =
    new ContravariantOps[F,A] { def self = v; implicit def F: Contravariant[F] = F0 }

  ////

  ////
}

trait ContravariantSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToContravariantOps[A](v: F[A]): ContravariantOps[F, A] = new ContravariantOps[F,A] { def self = v; implicit def F: Contravariant[F] = ContravariantSyntax.this.F }

  def F: Contravariant[F]
  ////

  ////
}
