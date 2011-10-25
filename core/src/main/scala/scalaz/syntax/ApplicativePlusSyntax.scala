package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ApplicativePlus` */
trait ApplicativePlusV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: ApplicativePlus[F]
  ////

  ////
}

trait ToApplicativePlusV extends ToApplicativeV with ToPlusV {
  implicit def ToApplicativePlusV[F[_],A](v: F[A])(implicit F0: ApplicativePlus[F]) =
    new ApplicativePlusV[F,A] { def self = v; implicit def F: ApplicativePlus[F] = F0 }
  implicit def ToApplicativePlusVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: ApplicativePlus[({type f[a] = F[X, a]})#f]) =
    new ApplicativePlusV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: ApplicativePlus[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToApplicativePlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: ApplicativePlus[({type f[a] = F[X, G, a]})#f]) =
    new ApplicativePlusV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: ApplicativePlus[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToApplicativePlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: ApplicativePlus[({type f[a] = F[X, Id, a]})#f]) =
    new ApplicativePlusV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: ApplicativePlus[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait ApplicativePlusSyntax[F[_]] extends ApplicativeSyntax[F] with PlusSyntax[F] {
  implicit def ToApplicativePlusV[A](v: F[A])(implicit F0: ApplicativePlus[F]): ApplicativePlusV[F, A] = new ApplicativePlusV[F,A] { def self = v; implicit def F: ApplicativePlus[F] = F0 }

  ////

  ////
}
