package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Pointed` */
trait PointedV[F[_],A] extends SyntaxV[F[A]] {
  ////
  ////
}

trait ToPointedSyntax extends ToFunctorSyntax {
  implicit def ToPointedV[F[_],A](v: F[A]) =
    (new PointedSyntax[F] {}).ToPointedV(v)
  implicit def ToPointedVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new PointedSyntax[({type f[a] = F[X, a]})#f] {}).ToPointedV(v)
  implicit def ToPointedVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new PointedSyntax[({type f[a] = F[X, G, a]})#f] {}).ToPointedV(v)
  implicit def ToPointedVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new PointedSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToPointedV(v)

  ////
  implicit def PointedIdV[A](v: => A) = new PointedIdV[A] {
    lazy val self = v
  }

  trait PointedIdV[A] extends SyntaxV[A] {
    def pure[F[_] : Pointed]: F[A] = Pointed[F].pure(self)
  }
  ////
}

trait PointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToPointedV[A](v: F[A]): PointedV[F, A] = new PointedV[F,A] { def self = v }

  ////
  def pure[A](a: => A)(implicit F: Pointed[F]): F[A] = F.pure(a)

  implicit def PointedIdV[A](v: => A) = new PointedIdV[A] {
    lazy val self = v
  }

  trait PointedIdV[A] extends SyntaxV[A] {
    def pure(implicit F: Pointed[F]): F[A] = Pointed[F].pure(self)
  }
  ////
}
