package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Applicative` */
sealed abstract class ApplicativeOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Applicative[F]
  ////
  final def unlessM(cond: Boolean): F[Unit] = scalaz.std.boolean.unlessM(cond)(self)
  final def whenM(cond: Boolean): F[Unit] = scalaz.std.boolean.whenM(cond)(self)
  final def replicateM(n: Int): F[List[A]] =
    F.replicateM(n, self)

  final def replicateM_(n: Int): F[Unit] =
    F.replicateM_(n, self)
  ////
}

trait ToApplicativeOps0 {
  implicit def ToApplicativeOpsUnapply[FA](v: FA)(implicit F0: Unapply[Applicative, FA]) =
    new ApplicativeOps[F0.M,F0.A] { def self = F0(v); implicit def F: Applicative[F0.M] = F0.TC }

}

trait ToApplicativeOps extends ToApplicativeOps0 with ToApplyOps {
  implicit def ToApplicativeOps[F[_],A](v: F[A])(implicit F0: Applicative[F]) =
    new ApplicativeOps[F,A] { def self = v; implicit def F: Applicative[F] = F0 }

  ////
  implicit def ApplicativeIdV[A](v: => A) = new ApplicativeIdV[A] {
    lazy val self = v
  }

  trait ApplicativeIdV[A] extends Ops[A] {
    def point[F[_] : Applicative]: F[A] = Applicative[F].point(self)
    def pure[F[_] : Applicative]: F[A] = Applicative[F].point(self)
    def η[F[_] : Applicative]: F[A] = Applicative[F].point(self)
  }  ////
}

trait ApplicativeSyntax[F[_]] extends ApplySyntax[F] {
  implicit def ToApplicativeOps[A](v: F[A]): ApplicativeOps[F, A] = new ApplicativeOps[F,A] { def self = v; implicit def F: Applicative[F] = ApplicativeSyntax.this.F }

  def F: Applicative[F]
  ////
  def point[A](a: => A)(implicit F: Applicative[F]): F[A] = F.point(a)

  /** Alias for `point` */
  def pure[A](a: => A)(implicit F: Applicative[F]): F[A] = F.point(a)
  def η[A](a: => A)(implicit F: Applicative[F]): F[A] = F.point(a)

  implicit def ApplicativeIdV[A](v: => A) = new ApplicativeIdV[A] {
    lazy val self = v
  }

  trait ApplicativeIdV[A] extends Ops[A] {
    def point(implicit F: Applicative[F]): F[A] = Applicative[F].point(self)

    /** Alias for `point` */
    def pure(implicit F: Applicative[F]): F[A] = Applicative[F].point(self)

    def η(implicit F: Applicative[F]): F[A] = Applicative[F].point(self)
  }

  ////
}
