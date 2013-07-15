package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Catchable` */
sealed abstract class CatchableOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Catchable[F]
  ////
  def attempt: F[Throwable \/ A] = F.attempt(self)
  ////
}

trait ToCatchableOps0 {
  implicit def ToCatchableOpsUnapply[FA](v: FA)(implicit F0: Unapply[Catchable, FA]) =
    new CatchableOps[F0.M,F0.A] { def self = F0(v); implicit def F: Catchable[F0.M] = F0.TC }

}

trait ToCatchableOps extends ToCatchableOps0 {
  implicit def ToCatchableOps[F[_],A](v: F[A])(implicit F0: Catchable[F]) =
    new CatchableOps[F,A] { def self = v; implicit def F: Catchable[F] = F0 }

  ////

  ////
}

trait CatchableSyntax[F[_]]  {
  implicit def ToCatchableOps[A](v: F[A]): CatchableOps[F, A] = new CatchableOps[F,A] { def self = v; implicit def F: Catchable[F] = CatchableSyntax.this.F }

  def F: Catchable[F]
  ////

  ////
}
