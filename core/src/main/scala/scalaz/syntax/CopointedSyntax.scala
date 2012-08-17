package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Copointed` */
trait CopointedOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Copointed[F]
  ////
  def copoint: A = F.copoint(self)

  ////
}

trait ToCopointedOps0 {
  implicit def ToCopointedOpsUnapply[FA](v: FA)(implicit F0: Unapply[Copointed, FA]) =
    new CopointedOps[F0.M,F0.A] { def self = F0(v); implicit def F: Copointed[F0.M] = F0.TC }

}

trait ToCopointedOps extends ToCopointedOps0 with ToFunctorOps {
  implicit def ToCopointedOps[F[_],A](v: F[A])(implicit F0: Copointed[F]) =
    new CopointedOps[F,A] { def self = v; implicit def F: Copointed[F] = F0 }

  ////

  ////
}

trait CopointedSyntax[F[_]] extends FunctorSyntax[F] { 
  implicit def ToCopointedOps[A](v: F[A]): CopointedOps[F, A] = new CopointedOps[F,A] { def self = v; implicit def F: Copointed[F] = CopointedSyntax.this.F }

  def F: Copointed[F]
  ////

  ////
}
