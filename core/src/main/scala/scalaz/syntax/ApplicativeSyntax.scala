package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Applicative` */
final class ApplicativeOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Applicative[F]) extends Ops[F[A]] {
  ////
  final def unlessM(cond: Boolean): F[Unit] = scalaz.std.boolean.unlessM(cond)(self)
  final def whenM(cond: Boolean): F[Unit] = scalaz.std.boolean.whenM(cond)(self)
  final def replicateM(n: Int): F[IList[A]] =
    F.replicateM(n, self)

  final def replicateM_(n: Int): F[Unit] =
    F.replicateM_(n, self)
  ////
}

sealed trait ToApplicativeOpsU[TC[F[_]] <: Applicative[F]] {
  implicit def ToApplicativeOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new ApplicativeOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToApplicativeOps0[TC[F[_]] <: Applicative[F]] extends ToApplicativeOpsU[TC] {
  implicit def ToApplicativeOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new ApplicativeOps[F,A](v)

  ////
  implicit def ApplicativeIdV[A](v: => A): ApplicativeIdV[A] = new ApplicativeIdV[A] {
    private[this] val nv = Need(v)
    def self = nv.value
  }

  trait ApplicativeIdV[A] extends Ops[A] {
    def point[F[_] : TC]: F[A] = Applicative[F].point(self)
    def pure[F[_] : TC]: F[A] = Applicative[F].point(self)
    def η[F[_] : TC]: F[A] = Applicative[F].point(self)
  }  ////
}

trait ToApplicativeOps[TC[F[_]] <: Applicative[F]] extends ToApplicativeOps0[TC] with ToApplyOps[TC] with ToInvariantApplicativeOps[TC]

trait ApplicativeSyntax[F[_]] extends ApplySyntax[F] with InvariantApplicativeSyntax[F] {
  implicit def ToApplicativeOps[A](v: F[A]): ApplicativeOps[F, A] = new ApplicativeOps[F,A](v)(ApplicativeSyntax.this.F)

  def F: Applicative[F]
  ////
  def point[A](a: => A)(implicit F: Applicative[F]): F[A] = F.point(a)

  /** Alias for `point` */
  def pure[A](a: => A)(implicit F: Applicative[F]): F[A] = F.point(a)
  def η[A](a: => A)(implicit F: Applicative[F]): F[A] = F.point(a)

  implicit def ApplicativeIdV[A](v: => A): ApplicativeIdV[A] = new ApplicativeIdV[A] {
    private[this] val vc = Need(v)
    def self = vc.value
  }

  trait ApplicativeIdV[A] extends Ops[A] {
    def point(implicit F: Applicative[F]): F[A] = Applicative[F].point(self)

    /** Alias for `point` */
    def pure(implicit F: Applicative[F]): F[A] = Applicative[F].point(self)

    def η(implicit F: Applicative[F]): F[A] = Applicative[F].point(self)
  }

  ////
}
