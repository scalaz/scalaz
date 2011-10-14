package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativePlus` */
trait ApplicativePlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToApplicativePlusSyntax extends ToApplicativeSyntax with ToPlusSyntax {
  implicit def ToApplicativePlusV[F[_],A](v: F[A]) =
    new ApplicativePlusV[F,A] { def self = v }
  implicit def ToApplicativePlusVFromBin[F[_, _], X, A](v: F[X, A]) =
    new ApplicativePlusV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToApplicativePlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new ApplicativePlusV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToApplicativePlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new ApplicativePlusV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait ApplicativePlusSyntax[F[_]] extends ApplicativeSyntax[F] with PlusSyntax[F] {
  implicit def ToApplicativePlusV[A](v: F[A]): ApplicativePlusV[F, A] = new ApplicativePlusV[F,A] { def self = v }

  ////

  ////
}
