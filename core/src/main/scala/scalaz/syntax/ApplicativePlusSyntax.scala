package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativePlus` */
trait ApplicativePlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToApplicativePlusSyntax extends ToApplicativeSyntax with ToPlusSyntax {
  implicit def ToApplicativePlusV[F[_],A](v: F[A]) =
    (new ApplicativePlusSyntax[F] {}).ToApplicativePlusV(v)
  implicit def ToApplicativePlusVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new ApplicativePlusSyntax[({type f[a] = F[X, a]})#f] {}).ToApplicativePlusV(v)
  implicit def ToApplicativePlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new ApplicativePlusSyntax[({type f[a] = F[X, G, a]})#f] {}).ToApplicativePlusV(v)
  implicit def ToApplicativePlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new ApplicativePlusSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToApplicativePlusV(v)

  ////

  ////
}

trait ApplicativePlusSyntax[F[_]] extends ApplicativeSyntax[F] with PlusSyntax[F] {
  implicit def ToApplicativePlusV[A](v: F[A]): ApplicativePlusV[F, A] = new ApplicativePlusV[F,A] { def self = v }

  ////

  ////
}
