package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Copointed` */
trait CopointedV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToCopointedSyntax extends ToContravariantSyntax {
  implicit def ToCopointedV[F[_],A](v: F[A]) =
    (new CopointedSyntax[F] {}).ToCopointedV(v)
  implicit def ToCopointedVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new CopointedSyntax[({type f[a] = F[X, a]})#f] {}).ToCopointedV(v)
  implicit def ToCopointedVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new CopointedSyntax[({type f[a] = F[X, G, a]})#f] {}).ToCopointedV(v)
  implicit def ToCopointedVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new CopointedSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToCopointedV(v)

  ////

  ////
}

trait CopointedSyntax[F[_]] extends ContravariantSyntax[F] {
  implicit def ToCopointedV[A](v: F[A]): CopointedV[F, A] = new CopointedV[F,A] { def self = v }

  ////

  ////
}
