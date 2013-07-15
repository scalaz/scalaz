package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `PlusEmpty` */
sealed abstract class PlusEmptyOps[F[_],A] extends Ops[F[A]] {
  implicit def F: PlusEmpty[F]
  ////

  ////
}

trait ToPlusEmptyOps0 {
  implicit def ToPlusEmptyOpsUnapply[FA](v: FA)(implicit F0: Unapply[PlusEmpty, FA]) =
    new PlusEmptyOps[F0.M,F0.A] { def self = F0(v); implicit def F: PlusEmpty[F0.M] = F0.TC }

}

trait ToPlusEmptyOps extends ToPlusEmptyOps0 with ToPlusOps {
  implicit def ToPlusEmptyOps[F[_],A](v: F[A])(implicit F0: PlusEmpty[F]) =
    new PlusEmptyOps[F,A] { def self = v; implicit def F: PlusEmpty[F] = F0 }

  ////

  ////
}

trait PlusEmptySyntax[F[_]] extends PlusSyntax[F] {
  implicit def ToPlusEmptyOps[A](v: F[A]): PlusEmptyOps[F, A] = new PlusEmptyOps[F,A] { def self = v; implicit def F: PlusEmpty[F] = PlusEmptySyntax.this.F }

  def F: PlusEmpty[F]
  ////

  ////
}
