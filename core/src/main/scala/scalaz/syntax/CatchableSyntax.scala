package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Catchable` */
final class CatchableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Catchable[F]) extends Ops[F[A]] {
  ////
  def attempt: F[Throwable \/ A] = F.attempt(self)
  ////
}

sealed trait ToCatchableOpsU[TC[F[_]] <: Catchable[F]] {
  implicit def ToCatchableOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): CatchableOps[F0.M, F0.A] =
    new CatchableOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCatchableOps0[TC[F[_]] <: Catchable[F]] extends ToCatchableOpsU[TC] {
  implicit def ToCatchableOps[F[_],A](v: F[A])(implicit F0: TC[F]): CatchableOps[F, A] =
    new CatchableOps[F,A](v)

  ////

  ////
}

trait ToCatchableOps[TC[F[_]] <: Catchable[F]] extends ToCatchableOps0[TC]

trait CatchableSyntax[F[_]]  {
  implicit def ToCatchableOps[A](v: F[A]): CatchableOps[F, A] = new CatchableOps[F,A](v)(CatchableSyntax.this.F)

  def F: Catchable[F]
  ////

  ////
}
