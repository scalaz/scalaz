package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
final class PlusOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Plus[F]) extends Ops[F[A]] {
  ////

  final def <+>(other: => F[A]) = F.plus(self, other)

  ////
}

sealed trait ToPlusOps0 {
  implicit def ToPlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[Plus, FA]) =
    new PlusOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToPlusOps extends ToPlusOps0 {
  implicit def ToPlusOps[F[_],A](v: F[A])(implicit F0: Plus[F]) =
    new PlusOps[F,A](v)

  ////

  ////
}

trait PlusSyntax[F[_]]  {
  implicit def ToPlusOps[A](v: F[A]): PlusOps[F, A] = new PlusOps[F,A](v)(PlusSyntax.this.F)

  def F: Plus[F]
  ////

  ////
}
