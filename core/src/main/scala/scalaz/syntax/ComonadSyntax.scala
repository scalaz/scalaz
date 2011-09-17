package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
trait ComonadV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToComonadSyntax extends ToCopointedSyntax with ToCojoinSyntax {
  implicit def ToComonadV[F[_],A](v: F[A]) =
    (new ComonadSyntax[F] {}).ToComonadV(v)
  implicit def ToComonadVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new ComonadSyntax[({type f[a] = F[X, a]})#f] {}).ToComonadV(v)
  implicit def ToComonadVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new ComonadSyntax[({type f[a] = F[X, G, a]})#f] {}).ToComonadV(v)
  implicit def ToComonadVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new ComonadSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToComonadV(v)

  ////

  ////
}

trait ComonadSyntax[F[_]] extends CopointedSyntax[F] with CojoinSyntax[F] {
  implicit def ToComonadV[A](v: F[A]): ComonadV[F, A] = new ComonadV[F,A] { def self = v }

  ////

  ////
}
