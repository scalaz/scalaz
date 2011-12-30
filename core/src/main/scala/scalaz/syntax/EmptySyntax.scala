package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Empty` */
trait EmptyV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Empty[F]
  ////

  ////
}

trait ToEmptyV0 {
  implicit def ToEmptyVUnapply[FA](v: FA)(implicit F0: Unapply[Empty, FA]) =
    new EmptyV[F0.M,F0.A] { def self = F0(v); implicit def F: Empty[F0.M] = F0.TC }

}

trait ToEmptyV extends ToEmptyV0 with ToPlusV {
  implicit def ToEmptyV[F[_],A](v: F[A])(implicit F0: Empty[F]) =
    new EmptyV[F,A] { def self = v; implicit def F: Empty[F] = F0 }

  ////

  ////
}

trait EmptySyntax[F[_]] extends PlusSyntax[F] {
  implicit def ToEmptyV[A](v: F[A])(implicit F0: Empty[F]): EmptyV[F, A] = new EmptyV[F,A] { def self = v; implicit def F: Empty[F] = F0 }

  ////

  ////
}
