package scalaz
package syntax
package effect

import scalaz.effect.MonadIO

/** Wraps a value `self` and provides methods related to `MonadIO` */
sealed abstract class MonadIOOps[F[_],A] extends Ops[F[A]] {
  implicit def F: MonadIO[F]
  ////

  ////
}

trait ToMonadIOOps0 {
  implicit def ToMonadIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadIO, FA]) =
    new MonadIOOps[F0.M,F0.A] { def self = F0(v); implicit def F: MonadIO[F0.M] = F0.TC }

}

trait ToMonadIOOps extends ToMonadIOOps0 with ToLiftIOOps with ToMonadOps {
  implicit def ToMonadIOOps[F[_],A](v: F[A])(implicit F0: MonadIO[F]) =
    new MonadIOOps[F,A] { def self = v; implicit def F: MonadIO[F] = F0 }

  ////

  ////
}

trait MonadIOSyntax[F[_]] extends LiftIOSyntax[F] with MonadSyntax[F] {
  implicit def ToMonadIOOps[A](v: F[A]): MonadIOOps[F, A] = new MonadIOOps[F,A] { def self = v; implicit def F: MonadIO[F] = MonadIOSyntax.this.F }

  def F: MonadIO[F]
  ////

  ////
}
