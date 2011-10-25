package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
trait MonadPlusV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: MonadPlus[F]
  ////

  ////
}

trait ToMonadPlusV extends ToMonadV with ToApplicativePlusV {
  implicit def ToMonadPlusV[F[_],A](v: F[A])(implicit F0: MonadPlus[F]) =
    new MonadPlusV[F,A] { def self = v; implicit def F: MonadPlus[F] = F0 }
  implicit def ToMonadPlusVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: MonadPlus[({type f[a] = F[X, a]})#f]) =
    new MonadPlusV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: MonadPlus[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToMonadPlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: MonadPlus[({type f[a] = F[X, G, a]})#f]) =
    new MonadPlusV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: MonadPlus[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToMonadPlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: MonadPlus[({type f[a] = F[X, Id, a]})#f]) =
    new MonadPlusV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: MonadPlus[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def ToMonadPlusV[A](v: F[A])(implicit F0: MonadPlus[F]): MonadPlusV[F, A] = new MonadPlusV[F,A] { def self = v; implicit def F: MonadPlus[F] = F0 }

  ////

  ////
}
