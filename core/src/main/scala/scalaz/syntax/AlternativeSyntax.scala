package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Alternative` */
trait AlternativeV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Alternative[F]
  ////
  final def orElse(a: => F[A]): F[A] = F.orElse(self, a)

  final def <|>(a: => F[A]): F[A] = F.orElse(self, a)
  
  final def oneOrMore: F[List[A]] = F.oneOrMore(self)
  
  final def zeroOrMore: F[List[A]] = F.zeroOrMore(self)
  ////
}

trait ToAlternativeV0 {
  implicit def ToAlternativeVUnapply[FA](v: FA)(implicit F0: Unapply[Alternative, FA]) =
    new AlternativeV[F0.M,F0.A] { def self = F0(v); implicit def F: Alternative[F0.M] = F0.TC }

}

trait ToAlternativeV extends ToAlternativeV0 with ToApplicativeV {
  implicit def ToAlternativeV[F[_],A](v: F[A])(implicit F0: Alternative[F]) =
    new AlternativeV[F,A] { def self = v; implicit def F: Alternative[F] = F0 }

  ////

  ////
}

trait AlternativeSyntax[F[_]] extends ApplicativeSyntax[F] {
  implicit def ToAlternativeV[A](v: F[A])(implicit F0: Alternative[F]): AlternativeV[F, A] = new AlternativeV[F,A] { def self = v; implicit def F: Alternative[F] = F0 }

  ////

  ////
}
