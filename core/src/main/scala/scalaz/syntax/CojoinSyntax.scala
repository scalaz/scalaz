package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cojoin` */
trait CojoinV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToCojoinSyntax  {
  implicit def cojoin[F[_],A](v: F[A]) =
    (new CojoinSyntax[F] {}).cojoinV(v)
  implicit def cojoinBin[F[_, _], X, A](v: F[X, A]) =
    (new CojoinSyntax[({type f[a] = F[X, a]})#f] {}).cojoinV(v)
  implicit def cojoinBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new CojoinSyntax[({type f[a] = F[X, G, a]})#f] {}).cojoinV(v)
  implicit def cojoinBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new CojoinSyntax[({type f[a] = F[X, Id, a]})#f] {}).cojoinV(v)
}

trait CojoinSyntax[F[_]]  {
  implicit def cojoinV[A](v: F[A]): CojoinV[F, A] = new CojoinV[F,A] { def self = v }

  ////

  ////
}
