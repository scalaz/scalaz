package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoBind` */
trait CoBindV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: CoBind[F]
  ////

  ////
}

trait ToCoBindV  {
  implicit def ToCoBindV[F[_],A](v: F[A])(implicit F0: CoBind[F]) =
    new CoBindV[F,A] { def self = v; implicit def F: CoBind[F] = F0 }
  implicit def ToCoBindVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: CoBind[({type f[a] = F[X, a]})#f]) =
    new CoBindV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: CoBind[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToCoBindVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: CoBind[({type f[a] = F[X, G, a]})#f]) =
    new CoBindV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: CoBind[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToCoBindVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: CoBind[({type f[a] = F[X, Id, a]})#f]) =
    new CoBindV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: CoBind[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait CoBindSyntax[F[_]]  {
  implicit def ToCoBindV[A](v: F[A])(implicit F0: CoBind[F]): CoBindV[F, A] = new CoBindV[F,A] { def self = v; implicit def F: CoBind[F] = F0 }

  ////

  ////
}
