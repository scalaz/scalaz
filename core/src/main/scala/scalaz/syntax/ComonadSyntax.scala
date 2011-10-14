package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
trait ComonadV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToComonadSyntax extends ToCopointedSyntax with ToCojoinSyntax {
  implicit def ToComonadV[F[_],A](v: F[A]) =
    new ComonadV[F,A] { def self = v }
  implicit def ToComonadVFromBin[F[_, _], X, A](v: F[X, A]) =
    new ComonadV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToComonadVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new ComonadV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToComonadVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new ComonadV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait ComonadSyntax[F[_]] extends CopointedSyntax[F] with CojoinSyntax[F] {
  implicit def ToComonadV[A](v: F[A]): ComonadV[F, A] = new ComonadV[F,A] { def self = v }

  ////

  ////
}
