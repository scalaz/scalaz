package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `PlusEmpty` */
trait PlusEmptyV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: PlusEmpty[F]
  ////

  ////
}

trait ToPlusEmptyV0 {
  implicit def ToPlusEmptyVUnapply[FA](v: FA)(implicit F0: Unapply[PlusEmpty, FA]) =
    new PlusEmptyV[F0.M,F0.A] { def self = F0(v); implicit def F: PlusEmpty[F0.M] = F0.TC }

}

trait ToPlusEmptyV extends ToPlusEmptyV0 with ToPlusV {
  implicit def ToPlusEmptyV[F[_],A](v: F[A])(implicit F0: PlusEmpty[F]) =
    new PlusEmptyV[F,A] { def self = v; implicit def F: PlusEmpty[F] = F0 }

  ////

  ////
}

trait PlusEmptySyntax[F[_]] extends PlusSyntax[F] {
  implicit def ToPlusEmptyV[A](v: F[A])(implicit F0: PlusEmpty[F]): PlusEmptyV[F, A] = new PlusEmptyV[F,A] { def self = v; implicit def F: PlusEmpty[F] = F0 }

  ////

  ////
}
