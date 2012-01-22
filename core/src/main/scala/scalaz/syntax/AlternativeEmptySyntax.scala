package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `AlternativeEmpty` */
trait AlternativeEmptyV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: AlternativeEmpty[F]
  ////

  ////
}

trait ToAlternativeEmptyV0 {
  implicit def ToAlternativeEmptyVUnapply[FA](v: FA)(implicit F0: Unapply[AlternativeEmpty, FA]) =
    new AlternativeEmptyV[F0.M,F0.A] { def self = F0(v); implicit def F: AlternativeEmpty[F0.M] = F0.TC }

}

trait ToAlternativeEmptyV extends ToAlternativeEmptyV0 with ToAlternativeV {
  implicit def ToAlternativeEmptyV[F[_],A](v: F[A])(implicit F0: AlternativeEmpty[F]) =
    new AlternativeEmptyV[F,A] { def self = v; implicit def F: AlternativeEmpty[F] = F0 }

  ////

  ////
}

trait AlternativeEmptySyntax[F[_]] extends AlternativeSyntax[F] {
  implicit def ToAlternativeEmptyV[A](v: F[A])(implicit F0: AlternativeEmpty[F]): AlternativeEmptyV[F, A] = new AlternativeEmptyV[F,A] { def self = v; implicit def F: AlternativeEmpty[F] = F0 }

  ////

  ////
}
