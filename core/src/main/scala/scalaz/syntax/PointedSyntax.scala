package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Pointed` */
trait PointedV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Pointed[F]
  ////
  ////
}

trait ToPointedV0 {
  implicit def ToPointedVUnapply[FA](v: FA)(implicit F0: Unapply[Pointed, FA]) =
    new PointedV[F0.M,F0.A] { def self = F0(v); implicit def F: Pointed[F0.M] = F0.TC }

}

trait ToPointedV extends ToPointedV0 with ToFunctorV {
  implicit def ToPointedV[F[_],A](v: F[A])(implicit F0: Pointed[F]) =
    new PointedV[F,A] { def self = v; implicit def F: Pointed[F] = F0 }

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
