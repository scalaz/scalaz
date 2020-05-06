package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Divisible` */
final class DivisibleOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Divisible[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDivisibleOpsU[TC[F[_]] <: Divisible[F]] {
  implicit def ToDivisibleOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): DivisibleOps[F0.M, F0.A] =
    new DivisibleOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToDivisibleOps0[TC[F[_]] <: Divisible[F]] extends ToDivisibleOpsU[TC] {
  implicit def ToDivisibleOps[F[_],A](v: F[A])(implicit F0: TC[F]): DivisibleOps[F, A] =
    new DivisibleOps[F, A](v)

  ////

  ////
}

trait ToDivisibleOps[TC[F[_]] <: Divisible[F]] extends ToDivisibleOps0[TC] with ToDivideOps[TC] with ToInvariantApplicativeOps[TC]

trait DivisibleSyntax[F[_]] extends DivideSyntax[F] with InvariantApplicativeSyntax[F] {
  implicit def ToDivisibleOps[A](v: F[A]): DivisibleOps[F, A] = new DivisibleOps[F,A](v)(DivisibleSyntax.this.F)

  def F: Divisible[F]
  ////

  ////
}
