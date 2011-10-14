package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Contravariant` */
trait ContravariantV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToContravariantSyntax  {
  implicit def ToContravariantV[F[_],A](v: F[A]) =
    new ContravariantV[F,A] { def self = v }
  implicit def ToContravariantVFromBin[F[_, _], X, A](v: F[X, A]) =
    new ContravariantV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToContravariantVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new ContravariantV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToContravariantVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new ContravariantV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait ContravariantSyntax[F[_]]  {
  implicit def ToContravariantV[A](v: F[A]): ContravariantV[F, A] = new ContravariantV[F,A] { def self = v }

  ////

  ////
}
