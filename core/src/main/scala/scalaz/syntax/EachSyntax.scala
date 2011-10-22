package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Each` */
trait EachV[F[_],A] extends SyntaxV[F[A]] {
  ////
  def foreach(f: A => Unit)(implicit F: Each[F]): Unit = F.each(self)(f)
  ////
}

trait ToEachSyntax  {
  implicit def ToEachV[F[_],A](v: F[A]) =
    new EachV[F,A] { def self = v }
  implicit def ToEachVFromBin[F[_, _], X, A](v: F[X, A]) =
    new EachV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToEachVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new EachV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToEachVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new EachV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait EachSyntax[F[_]]  {
  implicit def ToEachV[A](v: F[A]): EachV[F, A] = new EachV[F,A] { def self = v }

  ////

  ////
}
