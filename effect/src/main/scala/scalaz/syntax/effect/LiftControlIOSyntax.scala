package scalaz
package syntax
package effect

import scalaz.effect.LiftControlIO

/** Wraps a value `self` and provides methods related to `LiftControlIO` */
sealed abstract class LiftControlIOOps[F[_],A] extends Ops[F[A]] {
  implicit def F: LiftControlIO[F]
  ////

  ////
}

trait ToLiftControlIOOps0 {
  implicit def ToLiftControlIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[LiftControlIO, FA]) =
    new LiftControlIOOps[F0.M,F0.A] { def self = F0(v); implicit def F: LiftControlIO[F0.M] = F0.TC }

}

trait ToLiftControlIOOps extends ToLiftControlIOOps0 {
  implicit def ToLiftControlIOOps[F[_],A](v: F[A])(implicit F0: LiftControlIO[F]) =
    new LiftControlIOOps[F,A] { def self = v; implicit def F: LiftControlIO[F] = F0 }

  ////

  ////
}

trait LiftControlIOSyntax[F[_]]  {
  implicit def ToLiftControlIOOps[A](v: F[A]): LiftControlIOOps[F, A] = new LiftControlIOOps[F,A] { def self = v; implicit def F: LiftControlIO[F] = LiftControlIOSyntax.this.F }

  def F: LiftControlIO[F]
  ////

  ////
}
