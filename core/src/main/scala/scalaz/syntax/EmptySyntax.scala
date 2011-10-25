package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Empty` */
trait EmptyV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Empty[F]
  ////

  ////
}

trait ToEmptyV  {
  implicit def ToEmptyV[F[_],A](v: F[A])(implicit F0: Empty[F]) =
    new EmptyV[F,A] { def self = v; implicit def F: Empty[F] = F0 }
  implicit def ToEmptyVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Empty[({type f[a] = F[X, a]})#f]) =
    new EmptyV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Empty[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToEmptyVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Empty[({type f[a] = F[X, G, a]})#f]) =
    new EmptyV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Empty[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToEmptyVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Empty[({type f[a] = F[X, Id, a]})#f]) =
    new EmptyV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Empty[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait EmptySyntax[F[_]]  {
  implicit def ToEmptyV[A](v: F[A])(implicit F0: Empty[F]): EmptyV[F, A] = new EmptyV[F,A] { def self = v; implicit def F: Empty[F] = F0 }

  ////

  ////
}
