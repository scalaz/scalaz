package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monad` */
trait MonadV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToMonadSyntax extends ToApplicativeSyntax with ToBindSyntax {
  implicit def monad[F[_],A](v: F[A]) =
    (new MonadSyntax[F] {}).monadV(v)
  implicit def monadBin[F[_, _], X, A](v: F[X, A]) =
    (new MonadSyntax[({type f[a] = F[X, a]})#f] {}).monadV(v)
  implicit def monadBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new MonadSyntax[({type f[a] = F[X, G, a]})#f] {}).monadV(v)
  implicit def monadBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new MonadSyntax[({type f[a] = F[X, Id, a]})#f] {}).monadV(v)

  ////

  ////
}

trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F] {
  implicit def monadV[A](v: F[A]): MonadV[F, A] = new MonadV[F,A] { def self = v }

  ////

  ////
}
