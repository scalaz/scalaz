package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
sealed abstract class PlusOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Plus[F]
  ////

  final def <+>(other: => F[A]) = F.plus(self, other)

  ////
}

trait ToPlusOps0 {
  implicit def ToPlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[Plus, FA]) =
    new PlusOps[F0.M,F0.A] { def self = F0(v); implicit def F: Plus[F0.M] = F0.TC }

}

trait ToPlusOps extends ToPlusOps0 {
  implicit def ToPlusOps[F[_],A](v: F[A])(implicit F0: Plus[F]) =
    new PlusOps[F,A] { def self = v; implicit def F: Plus[F] = F0 }

  ////

  ////
}

trait PlusSyntax[F[_]]  {
  implicit def ToPlusOps[A](v: F[A]): PlusOps[F, A] = new PlusOps[F,A] { def self = v; implicit def F: Plus[F] = PlusSyntax.this.F }

  def F: Plus[F]
  ////

  ////
}
