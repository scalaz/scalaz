package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
trait ComonadV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Comonad[F]
  ////

  ////
}

trait ToComonadSyntax extends ToCopointedSyntax with ToCojoinSyntax {
  implicit def ToComonadV[F[_],A](v: F[A])(implicit F0: Comonad[F]) =
    new ComonadV[F,A] { def self = v; implicit def F: Comonad[F] = F0 }
  implicit def ToComonadVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Comonad[({type f[a] = F[X, a]})#f]) =
    new ComonadV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Comonad[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToComonadVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Comonad[({type f[a] = F[X, G, a]})#f]) =
    new ComonadV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Comonad[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToComonadVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Comonad[({type f[a] = F[X, Id, a]})#f]) =
    new ComonadV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Comonad[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait ComonadSyntax[F[_]] extends CopointedSyntax[F] with CojoinSyntax[F] {
  implicit def ToComonadV[A](v: F[A])(implicit F0: Comonad[F]): ComonadV[F, A] = new ComonadV[F,A] { def self = v; implicit def F: Comonad[F] = F0 }

  ////

  ////
}
