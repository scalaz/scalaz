package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Contravariant` */
trait ContravariantV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Contravariant[F]
  ////
  final def contramap[B](f: B => A): F[B] = F.contramap(self)(f)
  final def ∙[B](f: B => A): F[B] = F.contramap(self)(f)
  ////
}

trait ToContravariantV0 {
  implicit def ToContravariantVUnapply[FA](v: FA)(implicit F0: Unapply[Contravariant, FA]) =
    new ContravariantV[F0.M,F0.A] { def self = F0(v); implicit def F: Contravariant[F0.M] = F0.TC }

}

trait ToContravariantV extends ToContravariantV0 {
  implicit def ToContravariantV[F[_],A](v: F[A])(implicit F0: Contravariant[F]) =
    new ContravariantV[F,A] { def self = v; implicit def F: Contravariant[F] = F0 }

  ////

  ////
}

trait ContravariantSyntax[F[_]]  {
  implicit def ToContravariantV[A](v: F[A])(implicit F0: Contravariant[F]): ContravariantV[F, A] = new ContravariantV[F,A] { def self = v; implicit def F: Contravariant[F] = F0 }

  ////

  ////
}
