package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
trait MonadPlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToMonadPlusSyntax extends ToMonadSyntax with ToApplicativePlusSyntax {
  implicit def ToMonadPlusV[F[_],A](v: F[A]) =
    new MonadPlusV[F,A] { def self = v }
  implicit def ToMonadPlusVFromBin[F[_, _], X, A](v: F[X, A]) =
    new MonadPlusV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToMonadPlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new MonadPlusV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToMonadPlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new MonadPlusV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def ToMonadPlusV[A](v: F[A]): MonadPlusV[F, A] = new MonadPlusV[F,A] { def self = v }

  ////

  ////
}
