package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cojoin` */
trait CojoinV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Cojoin[F]
  ////

  ////
}

trait ToCojoinSyntax  {
  implicit def ToCojoinV[F[_],A](v: F[A])(implicit F0: Cojoin[F]) =
    new CojoinV[F,A] { def self = v; implicit def F: Cojoin[F] = F0 }
  implicit def ToCojoinVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Cojoin[({type f[a] = F[X, a]})#f]) =
    new CojoinV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Cojoin[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToCojoinVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Cojoin[({type f[a] = F[X, G, a]})#f]) =
    new CojoinV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Cojoin[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToCojoinVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Cojoin[({type f[a] = F[X, Id, a]})#f]) =
    new CojoinV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Cojoin[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait CojoinSyntax[F[_]]  {
  implicit def ToCojoinV[A](v: F[A])(implicit F0: Cojoin[F]): CojoinV[F, A] = new CojoinV[F,A] { def self = v; implicit def F: Cojoin[F] = F0 }

  ////

  ////
}
