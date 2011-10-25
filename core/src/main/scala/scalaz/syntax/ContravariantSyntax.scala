package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Contravariant` */
trait ContravariantV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Contravariant[F]
  ////

  ////
}

trait ToContravariantV  {
  implicit def ToContravariantV[F[_],A](v: F[A])(implicit F0: Contravariant[F]) =
    new ContravariantV[F,A] { def self = v; implicit def F: Contravariant[F] = F0 }
  implicit def ToContravariantVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Contravariant[({type f[a] = F[X, a]})#f]) =
    new ContravariantV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Contravariant[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToContravariantVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Contravariant[({type f[a] = F[X, G, a]})#f]) =
    new ContravariantV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Contravariant[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToContravariantVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Contravariant[({type f[a] = F[X, Id, a]})#f]) =
    new ContravariantV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Contravariant[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait ContravariantSyntax[F[_]]  {
  implicit def ToContravariantV[A](v: F[A])(implicit F0: Contravariant[F]): ContravariantV[F, A] = new ContravariantV[F,A] { def self = v; implicit def F: Contravariant[F] = F0 }

  ////

  ////
}
