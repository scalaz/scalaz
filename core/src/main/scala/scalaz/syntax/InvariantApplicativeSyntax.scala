package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `InvariantApplicative` */
final class InvariantApplicativeOps[F[_],A] private[syntax](val self: F[A])(implicit val F: InvariantApplicative[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToInvariantApplicativeOps0 {
  implicit def ToInvariantApplicativeOpsUnapply[FA](v: FA)(implicit F0: Unapply[InvariantApplicative, FA]) =
    new InvariantApplicativeOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToInvariantApplicativeOps extends ToInvariantApplicativeOps0 with ToInvariantFunctorOps {
  implicit def ToInvariantApplicativeOps[F[_],A](v: F[A])(implicit F0: InvariantApplicative[F]) =
    new InvariantApplicativeOps[F,A](v)

  ////

  ////
}

trait InvariantApplicativeSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToInvariantApplicativeOps[A](v: F[A]): InvariantApplicativeOps[F, A] = new InvariantApplicativeOps[F,A](v)(InvariantApplicativeSyntax.this.F)

  def F: InvariantApplicative[F]
  ////

  ////
}
