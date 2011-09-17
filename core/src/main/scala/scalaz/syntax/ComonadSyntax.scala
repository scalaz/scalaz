package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
trait ComonadV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToComonadSyntax extends ToCopointedSyntax with ToCojoinSyntax {
  implicit def comonad[F[_],A](v: F[A]) =
    (new ComonadSyntax[F] {}).comonadV(v)
  implicit def comonadBin[F[_, _], X, A](v: F[X, A]) =
    (new ComonadSyntax[({type f[a] = F[X, a]})#f] {}).comonadV(v)
  implicit def comonadBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new ComonadSyntax[({type f[a] = F[X, G, a]})#f] {}).comonadV(v)
  implicit def comonadBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new ComonadSyntax[({type f[a] = F[X, Id, a]})#f] {}).comonadV(v)
}

trait ComonadSyntax[F[_]] extends CopointedSyntax[F] with CojoinSyntax[F] {
  implicit def comonadV[A](v: F[A]): ComonadV[F, A] = new ComonadV[F,A] { def self = v }

  ////

  ////
}
