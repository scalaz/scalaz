package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cozip` */
sealed abstract class CozipOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Cozip[F]
  ////

  ////
}

trait ToCozipOps0 {
  implicit def ToCozipOpsUnapply[FA](v: FA)(implicit F0: Unapply[Cozip, FA]) =
    new CozipOps[F0.M,F0.A] { def self = F0(v); implicit def F: Cozip[F0.M] = F0.TC }

}

trait ToCozipOps extends ToCozipOps0 {
  implicit def ToCozipOps[F[_],A](v: F[A])(implicit F0: Cozip[F]) =
    new CozipOps[F,A] { def self = v; implicit def F: Cozip[F] = F0 }

  ////

  ////
}

trait CozipSyntax[F[_]]  {
  implicit def ToCozipOps[A](v: F[A]): CozipOps[F, A] = new CozipOps[F,A] { def self = v; implicit def F: Cozip[F] = CozipSyntax.this.F }

  def F: Cozip[F]
  ////

  ////
}
