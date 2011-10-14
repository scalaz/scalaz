package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monad` */
trait MonadV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToMonadSyntax extends ToApplicativeSyntax with ToBindSyntax {
  implicit def ToMonadV[F[_],A](v: F[A]) =
    new MonadV[F,A] { def self = v }
  implicit def ToMonadVFromBin[F[_, _], X, A](v: F[X, A]) =
    new MonadV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToMonadVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new MonadV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToMonadVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new MonadV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F] {
  implicit def ToMonadV[A](v: F[A]): MonadV[F, A] = new MonadV[F,A] { def self = v }

  ////

  ////
}
