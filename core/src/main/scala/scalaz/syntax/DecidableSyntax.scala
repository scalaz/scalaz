package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Decidable` */
final class DecidableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Decidable[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDecidableOps0 {
  implicit def ToDecidableOpsUnapply[FA](v: FA)(implicit F0: Unapply[Decidable, FA]) =
    new DecidableOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToDecidableOps extends ToDecidableOps0 with ToDivisibleOps with ToInvariantAltOps {
  implicit def ToDecidableOps[F[_],A](v: F[A])(implicit F0: Decidable[F]) =
    new DecidableOps[F,A](v)

  ////

  ////
}

trait DecidableSyntax[F[_]] extends DivisibleSyntax[F] with InvariantAltSyntax[F] {
  implicit def ToDecidableOps[A](v: F[A]): DecidableOps[F, A] = new DecidableOps[F,A](v)(DecidableSyntax.this.F)

  def F: Decidable[F]
  ////

  ////
}
