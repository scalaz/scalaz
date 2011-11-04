package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cobind` */
trait CobindV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Cobind[F]
  ////

  ////
}

trait ToCobindV  {
  implicit def ToCobindV[F[_],A](v: F[A])(implicit F0: Cobind[F]) =
    new CobindV[F,A] { def self = v; implicit def F: Cobind[F] = F0 }
  implicit def ToCobindVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Cobind[({type f[a] = F[X, a]})#f]) =
    new CobindV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Cobind[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToCobindVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Cobind[({type f[a] = F[X, G, a]})#f]) =
    new CobindV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Cobind[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToCobindVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Cobind[({type f[a] = F[X, Id, a]})#f]) =
    new CobindV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Cobind[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait CobindSyntax[F[_]]  {
  implicit def ToCobindV[A](v: F[A])(implicit F0: Cobind[F]): CobindV[F, A] = new CobindV[F,A] { def self = v; implicit def F: Cobind[F] = F0 }

  ////

  ////
}
