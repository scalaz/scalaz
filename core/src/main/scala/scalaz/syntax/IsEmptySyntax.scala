package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `IsEmpty` */
sealed abstract class IsEmptyOps[F[_],A] extends Ops[F[A]] {
  implicit def F: IsEmpty[F]
  ////

  ////
}

trait ToIsEmptyOps0 {
  implicit def ToIsEmptyOpsUnapply[FA](v: FA)(implicit F0: Unapply[IsEmpty, FA]) =
    new IsEmptyOps[F0.M,F0.A] { def self = F0(v); implicit def F: IsEmpty[F0.M] = F0.TC }

}

trait ToIsEmptyOps extends ToIsEmptyOps0 with ToPlusEmptyOps {
  implicit def ToIsEmptyOps[F[_],A](v: F[A])(implicit F0: IsEmpty[F]) =
    new IsEmptyOps[F,A] { def self = v; implicit def F: IsEmpty[F] = F0 }

  ////

  ////
}

trait IsEmptySyntax[F[_]] extends PlusEmptySyntax[F] {
  implicit def ToIsEmptyOps[A](v: F[A]): IsEmptyOps[F, A] = new IsEmptyOps[F,A] { def self = v; implicit def F: IsEmpty[F] = IsEmptySyntax.this.F }

  def F: IsEmpty[F]
  ////

  ////
}
