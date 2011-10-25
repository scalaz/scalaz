package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Length` */
trait LengthV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Length[F]
  ////

  ////
}

trait ToLengthV  {
  implicit def ToLengthV[F[_],A](v: F[A])(implicit F0: Length[F]) =
    new LengthV[F,A] { def self = v; implicit def F: Length[F] = F0 }
  implicit def ToLengthVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Length[({type f[a] = F[X, a]})#f]) =
    new LengthV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Length[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToLengthVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Length[({type f[a] = F[X, G, a]})#f]) =
    new LengthV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Length[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToLengthVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Length[({type f[a] = F[X, Id, a]})#f]) =
    new LengthV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Length[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait LengthSyntax[F[_]]  {
  implicit def ToLengthV[A](v: F[A])(implicit F0: Length[F]): LengthV[F, A] = new LengthV[F,A] { def self = v; implicit def F: Length[F] = F0 }

  ////

  ////
}
