package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Copointed` */
trait CopointedV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Copointed[F]
  ////
  def copoint: A = F.copoint(self)

  ////
}

trait ToCopointedV0 {
  implicit def ToCopointedVUnapply[FA](v: FA)(implicit F0: Unapply[Copointed, FA]) =
    new CopointedV[F0.M,F0.A] { def self = F0(v); implicit def F: Copointed[F0.M] = F0.TC }

}

trait ToCopointedV extends ToCopointedV0 with ToFunctorV {
  implicit def ToCopointedV[F[_],A](v: F[A])(implicit F0: Copointed[F]) =
    new CopointedV[F,A] { def self = v; implicit def F: Copointed[F] = F0 }

  ////

  ////
}

trait CopointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCopointedV[A](v: F[A])(implicit F0: Copointed[F]): CopointedV[F, A] = new CopointedV[F,A] { def self = v; implicit def F: Copointed[F] = F0 }

  ////

  ////
}
