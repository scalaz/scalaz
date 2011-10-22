package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Index` */
trait IndexV[F[_],A] extends SyntaxV[F[A]] {
  ////
  def index(n: Int)(implicit F: Index[F]): Option[A] = F.index(self, n)
  def indexOr(default: => A, n: Int)(implicit F: Index[F]): A = F.indexOr(self, default, n)
  ////
}

trait ToIndexSyntax  {
  implicit def ToIndexV[F[_],A](v: F[A]) =
    new IndexV[F,A] { def self = v }
  implicit def ToIndexVFromBin[F[_, _], X, A](v: F[X, A]) =
    new IndexV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToIndexVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new IndexV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToIndexVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new IndexV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait IndexSyntax[F[_]]  {
  implicit def ToIndexV[A](v: F[A]): IndexV[F, A] = new IndexV[F,A] { def self = v }

  ////

  ////
}
