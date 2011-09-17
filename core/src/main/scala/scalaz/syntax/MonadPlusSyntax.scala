package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
trait MonadPlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToMonadPlusSyntax extends ToMonadSyntax with ToApplicativePlusSyntax {
  implicit def monadPlus[F[_],A](v: F[A]) =
    (new MonadPlusSyntax[F] {}).monadPlusV(v)
  implicit def monadPlusBin[F[_, _], X, A](v: F[X, A]) =
    (new MonadPlusSyntax[({type f[a] = F[X, a]})#f] {}).monadPlusV(v)
  implicit def monadPlusBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new MonadPlusSyntax[({type f[a] = F[X, G, a]})#f] {}).monadPlusV(v)
  implicit def monadPlusBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new MonadPlusSyntax[({type f[a] = F[X, Id, a]})#f] {}).monadPlusV(v)
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def monadPlusV[A](v: F[A]): MonadPlusV[F, A] = new MonadPlusV[F,A] { def self = v }

  ////

  ////
}
