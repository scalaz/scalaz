package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoPointed` */
trait CoPointedV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: CoPointed[F]
  ////

  ////
}

trait ToCoPointedV extends ToFunctorV {
  implicit def ToCoPointedV[FA](v: FA)(implicit F0: Unapply[CoPointed, FA]) =
    new CoPointedV[F0.M,F0.A] { def self = F0(v); implicit def F: CoPointed[F0.M] = F0.TC }

  ////

  ////
}

trait CoPointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCoPointedV[A](v: F[A])(implicit F0: CoPointed[F]): CoPointedV[F, A] = new CoPointedV[F,A] { def self = v; implicit def F: CoPointed[F] = F0 }

  ////

  ////
}
