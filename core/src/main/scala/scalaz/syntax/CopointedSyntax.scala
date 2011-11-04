package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Copointed` */
trait CopointedV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Copointed[F]
  ////

  ////
}

trait ToCopointedV extends ToFunctorV {
  implicit def ToCopointedV[F[_],A](v: F[A])(implicit F0: Copointed[F]) =
    new CopointedV[F,A] { def self = v; implicit def F: Copointed[F] = F0 }
  implicit def ToCopointedVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Copointed[({type f[a] = F[X, a]})#f]) =
    new CopointedV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Copointed[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToCopointedVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Copointed[({type f[a] = F[X, G, a]})#f]) =
    new CopointedV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Copointed[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToCopointedVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Copointed[({type f[a] = F[X, Id, a]})#f]) =
    new CopointedV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Copointed[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait CopointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCopointedV[A](v: F[A])(implicit F0: Copointed[F]): CopointedV[F, A] = new CopointedV[F,A] { def self = v; implicit def F: Copointed[F] = F0 }

  ////

  ////
}
