package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativePlus` */
trait ApplicativePlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToApplicativePlusSyntax extends ToApplicativeSyntax with ToPlusSyntax {
  implicit def applicativePlus[F[_],A](v: F[A]) =
    (new ApplicativePlusSyntax[F] {}).applicativePlusV(v)
  implicit def applicativePlusBin[F[_, _], X, A](v: F[X, A]) =
    (new ApplicativePlusSyntax[({type f[a] = F[X, a]})#f] {}).applicativePlusV(v)
  implicit def applicativePlusBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new ApplicativePlusSyntax[({type f[a] = F[X, G, a]})#f] {}).applicativePlusV(v)
  implicit def applicativePlusBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new ApplicativePlusSyntax[({type f[a] = F[X, Id, a]})#f] {}).applicativePlusV(v)
}

trait ApplicativePlusSyntax[F[_]] extends ApplicativeSyntax[F] with PlusSyntax[F] {
  implicit def applicativePlusV[A](v: F[A]): ApplicativePlusV[F, A] = new ApplicativePlusV[F,A] { def self = v }

  ////

  ////
}
