package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Copointed` */
trait CopointedV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToCopointedSyntax  {
  implicit def ToCopointedV[F[_],A](v: F[A]) =
    new CopointedV[F,A] { def self = v }
  implicit def ToCopointedVFromBin[F[_, _], X, A](v: F[X, A]) =
    new CopointedV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToCopointedVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new CopointedV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToCopointedVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new CopointedV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait CopointedSyntax[F[_]]  {
  implicit def ToCopointedV[A](v: F[A]): CopointedV[F, A] = new CopointedV[F,A] { def self = v }

  ////

  ////
}
