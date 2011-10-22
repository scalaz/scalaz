package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Length` */
trait LengthV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToLengthSyntax  {
  implicit def ToLengthV[F[_],A](v: F[A]) =
    new LengthV[F,A] { def self = v }
  implicit def ToLengthVFromBin[F[_, _], X, A](v: F[X, A]) =
    new LengthV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToLengthVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new LengthV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToLengthVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new LengthV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait LengthSyntax[F[_]]  {
  implicit def ToLengthV[A](v: F[A]): LengthV[F, A] = new LengthV[F,A] { def self = v }

  ////

  ////
}
