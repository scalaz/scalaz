package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Pointed` */
trait PointedOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Pointed[F]
  ////
  ////
}

trait ToPointedOps0 {
  implicit def ToPointedOpsUnapply[FA](v: FA)(implicit F0: Unapply[Pointed, FA]) =
    new PointedOps[F0.M,F0.A] { def self = F0(v); implicit def F: Pointed[F0.M] = F0.TC }

}

trait ToPointedOps extends ToPointedOps0 with ToFunctorOps {
  implicit def ToPointedOps[F[_],A](v: F[A])(implicit F0: Pointed[F]) =
    new PointedOps[F,A] { def self = v; implicit def F: Pointed[F] = F0 }

  ////
  implicit def PointedIdV[A](v: => A) = new PointedIdV[A] {
    lazy val self = v
  }

  trait PointedIdV[A] extends Ops[A] {
    def point[F[_] : Pointed]: F[A] = Pointed[F].point(self)
    def pure[F[_] : Pointed]: F[A] = Pointed[F].point(self)
    def η[F[_] : Pointed]: F[A] = Pointed[F].point(self)
  }
  ////
}

trait PointedSyntax[F[_]] extends FunctorSyntax[F] { 
  implicit def ToPointedOps[A](v: F[A]): PointedOps[F, A] = new PointedOps[F,A] { def self = v; implicit def F: Pointed[F] = PointedSyntax.this.F }

  def F: Pointed[F]
  ////
  def point[A](a: => A)(implicit F: Pointed[F]): F[A] = F.point(a)

  /** Alias for `point` */
  def pure[A](a: => A)(implicit F: Pointed[F]): F[A] = F.point(a)
  def η[A](a: => A)(implicit F: Pointed[F]): F[A] = F.point(a)

  implicit def PointedIdV[A](v: => A) = new PointedIdV[A] {
    lazy val self = v
  }

  trait PointedIdV[A] extends Ops[A] {
    def point(implicit F: Pointed[F]): F[A] = Pointed[F].point(self)

    /** Alias for `point` */
    def pure(implicit F: Pointed[F]): F[A] = Pointed[F].point(self)

    def η(implicit F: Pointed[F]): F[A] = Pointed[F].point(self)
  }
  ////
}
