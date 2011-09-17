package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Pointed` */
trait PointedV[F[_],A] extends SyntaxV[F[A]] {
  ////
  def pure[F[_]](implicit F: Pointed[F]) = F.pure(self)
  ////
}

trait ToPointedSyntax extends ToFunctorSyntax {
  implicit def pointed[F[_],A](v: F[A]) =
    (new PointedSyntax[F] {}).pointedV(v)
  implicit def pointedBin[F[_, _], X, A](v: F[X, A]) =
    (new PointedSyntax[({type f[a] = F[X, a]})#f] {}).pointedV(v)
  implicit def pointedBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new PointedSyntax[({type f[a] = F[X, G, a]})#f] {}).pointedV(v)
  implicit def pointedBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new PointedSyntax[({type f[a] = F[X, Id, a]})#f] {}).pointedV(v)
}

trait PointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def pointedV[A](v: F[A]): PointedV[F, A] = new PointedV[F,A] { def self = v }

  ////

  ////
}
