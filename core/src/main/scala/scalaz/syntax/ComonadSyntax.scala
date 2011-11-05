package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoMonad` */
trait CoMonadV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: CoMonad[F]
  ////

  ////
}

trait ToCoMonadV extends ToCoPointedV with ToCoJoinV with ToCoBindV {
  implicit def ToCoMonadV[F[_],A](v: F[A])(implicit F0: CoMonad[F]) =
    new CoMonadV[F,A] { def self = v; implicit def F: CoMonad[F] = F0 }
  implicit def ToCoMonadVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: CoMonad[({type f[a] = F[X, a]})#f]) =
    new CoMonadV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: CoMonad[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToCoMonadVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: CoMonad[({type f[a] = F[X, G, a]})#f]) =
    new CoMonadV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: CoMonad[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToCoMonadVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: CoMonad[({type f[a] = F[X, Id, a]})#f]) =
    new CoMonadV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: CoMonad[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait CoMonadSyntax[F[_]] extends CoPointedSyntax[F] with CoJoinSyntax[F] with CoBindSyntax[F] {
  implicit def ToCoMonadV[A](v: F[A])(implicit F0: CoMonad[F]): CoMonadV[F, A] = new CoMonadV[F,A] { def self = v; implicit def F: CoMonad[F] = F0 }

  ////

  ////
}
