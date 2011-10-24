package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Each` */
trait EachV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Each[F]
  ////
  def foreach(f: A => Unit): Unit = F.each(self)(f)
  ////
}

trait ToEachSyntax  {
  implicit def ToEachV[F[_],A](v: F[A])(implicit F0: Each[F]) =
    new EachV[F,A] { def self = v; implicit def F: Each[F] = F0 }
  implicit def ToEachVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Each[({type f[a] = F[X, a]})#f]) =
    new EachV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Each[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToEachVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Each[({type f[a] = F[X, G, a]})#f]) =
    new EachV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Each[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToEachVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Each[({type f[a] = F[X, Id, a]})#f]) =
    new EachV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Each[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait EachSyntax[F[_]]  {
  implicit def ToEachV[A](v: F[A])(implicit F0: Each[F]): EachV[F, A] = new EachV[F,A] { def self = v; implicit def F: Each[F] = F0 }

  ////

  ////
}
