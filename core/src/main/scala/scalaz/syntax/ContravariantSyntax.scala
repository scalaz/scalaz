package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Contravariant` */
trait ContravariantV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToContravariantSyntax  {
  implicit def contravariant[F[_],A](v: F[A]) =
    (new ContravariantSyntax[F] {}).contravariantV(v)
  implicit def contravariantBin[F[_, _], X, A](v: F[X, A]) =
    (new ContravariantSyntax[({type f[a] = F[X, a]})#f] {}).contravariantV(v)
  implicit def contravariantBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new ContravariantSyntax[({type f[a] = F[X, G, a]})#f] {}).contravariantV(v)
  implicit def contravariantBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new ContravariantSyntax[({type f[a] = F[X, Id, a]})#f] {}).contravariantV(v)

  ////

  ////
}

trait ContravariantSyntax[F[_]]  {
  implicit def contravariantV[A](v: F[A]): ContravariantV[F, A] = new ContravariantV[F,A] { def self = v }

  ////

  ////
}
