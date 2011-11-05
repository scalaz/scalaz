package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Pointed` */
trait PointedV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Pointed[F]
  ////
  ////
}

trait ToPointedV extends ToFunctorV {
  implicit def ToPointedV[F[_],A](v: F[A])(implicit F0: Pointed[F]) =
    new PointedV[F,A] { def self = v; implicit def F: Pointed[F] = F0 }
  implicit def ToPointedVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Pointed[({type f[a] = F[X, a]})#f]) =
    new PointedV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Pointed[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToPointedVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Pointed[({type f[a] = F[X, G, a]})#f]) =
    new PointedV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Pointed[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToPointedVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Pointed[({type f[a] = F[X, Id, a]})#f]) =
    new PointedV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Pointed[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////
  implicit def PointedIdV[A](v: => A) = new PointedIdV[A] {
    lazy val self = v
  }

  trait PointedIdV[A] extends SyntaxV[A] {
    def point[F[_] : Pointed]: F[A] = Pointed[F].point(self)
  }
  ////
}

trait PointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToPointedV[A](v: F[A])(implicit F0: Pointed[F]): PointedV[F, A] = new PointedV[F,A] { def self = v; implicit def F: Pointed[F] = F0 }

  ////
  def point[A](a: => A)(implicit F: Pointed[F]): F[A] = F.point(a)

  implicit def PointedIdV[A](v: => A) = new PointedIdV[A] {
    lazy val self = v
  }

  trait PointedIdV[A] extends SyntaxV[A] {
    def point(implicit F: Pointed[F]): F[A] = Pointed[F].point(self)
  }
  ////
}
