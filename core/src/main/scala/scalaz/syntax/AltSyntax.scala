package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Alt` */
final class AltOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Alt[F]) extends Ops[F[A]] {
  ////

  // Haskell uses <!> but that's not valid scala syntax...
  def <|>(that: => F[A]): F[A] = F.alt(self, that)

  ////
}

sealed trait ToAltOpsU[TC[F[_]] <: Alt[F]] {
  implicit def ToAltOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new AltOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToAltOps0[TC[F[_]] <: Alt[F]] extends ToAltOpsU[TC] {
  implicit def ToAltOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new AltOps[F,A](v)

  ////

  ////
}

trait ToAltOps[TC[F[_]] <: Alt[F]] extends ToAltOps0[TC] with ToApplicativeOps[TC] with ToInvariantAltOps[TC]

trait AltSyntax[F[_]] extends ApplicativeSyntax[F] with InvariantAltSyntax[F] {
  implicit def ToAltOps[A](v: F[A]): AltOps[F, A] = new AltOps[F,A](v)(AltSyntax.this.F)

  def F: Alt[F]
  ////

  ////
}
