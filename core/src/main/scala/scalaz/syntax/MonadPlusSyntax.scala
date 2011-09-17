package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
trait MonadPlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToMonadPlusSyntax extends ToMonadSyntax with ToApplicativePlusSyntax {
  implicit def ToMonadPlusV[F[_],A](v: F[A]) =
    (new MonadPlusSyntax[F] {}).ToMonadPlusV(v)
  implicit def ToMonadPlusVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new MonadPlusSyntax[({type f[a] = F[X, a]})#f] {}).ToMonadPlusV(v)
  implicit def ToMonadPlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new MonadPlusSyntax[({type f[a] = F[X, G, a]})#f] {}).ToMonadPlusV(v)
  implicit def ToMonadPlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new MonadPlusSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToMonadPlusV(v)

  ////

  ////
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def ToMonadPlusV[A](v: F[A]): MonadPlusV[F, A] = new MonadPlusV[F,A] { def self = v }

  ////

  ////
}
