package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cojoin` */
trait CojoinV[F[_],A] extends SyntaxV[F[A]] {
  ////

  ////
}

trait ToCojoinSyntax  {
  implicit def ToCojoinV[F[_],A](v: F[A]) =
    (new CojoinSyntax[F] {}).ToCojoinV(v)
  implicit def ToCojoinVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new CojoinSyntax[({type f[a] = F[X, a]})#f] {}).ToCojoinV(v)
  implicit def ToCojoinVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new CojoinSyntax[({type f[a] = F[X, G, a]})#f] {}).ToCojoinV(v)
  implicit def ToCojoinVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new CojoinSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToCojoinV(v)

  ////

  ////
}

trait CojoinSyntax[F[_]]  {
  implicit def ToCojoinV[A](v: F[A]): CojoinV[F, A] = new CojoinV[F,A] { def self = v }

  ////

  ////
}
