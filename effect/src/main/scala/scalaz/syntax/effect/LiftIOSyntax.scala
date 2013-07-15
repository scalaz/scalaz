package scalaz
package syntax
package effect

import scalaz.effect.LiftIO

/** Wraps a value `self` and provides methods related to `LiftIO` */
sealed abstract class LiftIOOps[F[_],A] extends Ops[F[A]] {
  implicit def F: LiftIO[F]
  ////
  
  ////
}

trait ToLiftIOOps0 {
  implicit def ToLiftIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[LiftIO, FA]) =
    new LiftIOOps[F0.M,F0.A] { def self = F0(v); implicit def F: LiftIO[F0.M] = F0.TC }

}

trait ToLiftIOOps extends ToLiftIOOps0 {
  implicit def ToLiftIOOps[F[_],A](v: F[A])(implicit F0: LiftIO[F]) =
    new LiftIOOps[F,A] { def self = v; implicit def F: LiftIO[F] = F0 }

  ////

  ////
}

trait LiftIOSyntax[F[_]]  {
  implicit def ToLiftIOOps[A](v: F[A]): LiftIOOps[F, A] = new LiftIOOps[F,A] { def self = v; implicit def F: LiftIO[F] = LiftIOSyntax.this.F }

  def F: LiftIO[F]
  ////

  ////
}
