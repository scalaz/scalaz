package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monad` */
trait MonadV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Monad[F]
  ////

  ////
}

trait ToMonadSyntax extends ToApplicativeSyntax with ToBindSyntax {
  implicit def ToMonadV[F[_],A](v: F[A])(implicit F0: Monad[F]) =
    new MonadV[F,A] { def self = v; implicit def F: Monad[F] = F0 }
  implicit def ToMonadVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Monad[({type f[a] = F[X, a]})#f]) =
    new MonadV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Monad[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToMonadVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Monad[({type f[a] = F[X, G, a]})#f]) =
    new MonadV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Monad[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToMonadVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Monad[({type f[a] = F[X, Id, a]})#f]) =
    new MonadV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Monad[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F] {
  implicit def ToMonadV[A](v: F[A])(implicit F0: Monad[F]): MonadV[F, A] = new MonadV[F,A] { def self = v; implicit def F: Monad[F] = F0 }

  ////

  ////
}
