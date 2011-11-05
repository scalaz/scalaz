package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoPointed` */
trait CoPointedV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: CoPointed[F]
  ////

  ////
}

trait ToCoPointedV extends ToFunctorV {
  implicit def ToCoPointedV[F[_],A](v: F[A])(implicit F0: CoPointed[F]) =
    new CoPointedV[F,A] { def self = v; implicit def F: CoPointed[F] = F0 }
  implicit def ToCoPointedVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: CoPointed[({type f[a] = F[X, a]})#f]) =
    new CoPointedV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: CoPointed[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToCoPointedVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: CoPointed[({type f[a] = F[X, G, a]})#f]) =
    new CoPointedV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: CoPointed[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToCoPointedVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: CoPointed[({type f[a] = F[X, Id, a]})#f]) =
    new CoPointedV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: CoPointed[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait CoPointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCoPointedV[A](v: F[A])(implicit F0: CoPointed[F]): CoPointedV[F, A] = new CoPointedV[F,A] { def self = v; implicit def F: CoPointed[F] = F0 }

  ////

  ////
}
