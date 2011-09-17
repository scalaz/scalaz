package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Copointed` */
trait CopointedV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToCopointedSyntax extends ToContravariantSyntax {
  implicit def copointed[F[_],A](v: F[A]) =
    (new CopointedSyntax[F] {}).copointedV(v)
  implicit def copointedBin[F[_, _], X, A](v: F[X, A]) =
    (new CopointedSyntax[({type f[a] = F[X, a]})#f] {}).copointedV(v)
  implicit def copointedBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new CopointedSyntax[({type f[a] = F[X, G, a]})#f] {}).copointedV(v)
  implicit def copointedBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new CopointedSyntax[({type f[a] = F[X, Id, a]})#f] {}).copointedV(v)

  ////

  ////
}

trait CopointedSyntax[F[_]] extends ContravariantSyntax[F] {
  implicit def copointedV[A](v: F[A]): CopointedV[F, A] = new CopointedV[F,A] { def self = v }

  ////

  ////
}
