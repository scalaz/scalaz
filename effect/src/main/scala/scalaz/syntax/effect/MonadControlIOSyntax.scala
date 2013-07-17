package scalaz
package syntax
package effect

import scalaz.effect.MonadControlIO

/** Wraps a value `self` and provides methods related to `MonadControlIO` */
sealed abstract class MonadControlIOOps[F[_],A] extends Ops[F[A]] {
  implicit def F: MonadControlIO[F]
  ////

  ////
}

trait ToMonadControlIOOps0 {
  implicit def ToMonadControlIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadControlIO, FA]) =
    new MonadControlIOOps[F0.M,F0.A] { def self = F0(v); implicit def F: MonadControlIO[F0.M] = F0.TC }

}

trait ToMonadControlIOOps extends ToMonadControlIOOps0 with ToLiftControlIOOps with ToMonadOps {
  implicit def ToMonadControlIOOps[F[_],A](v: F[A])(implicit F0: MonadControlIO[F]) =
    new MonadControlIOOps[F,A] { def self = v; implicit def F: MonadControlIO[F] = F0 }

  ////

  ////
}

trait MonadControlIOSyntax[F[_]] extends LiftControlIOSyntax[F] with MonadSyntax[F] {
  implicit def ToMonadControlIOOps[A](v: F[A]): MonadControlIOOps[F, A] = new MonadControlIOOps[F,A] { def self = v; implicit def F: MonadControlIO[F] = MonadControlIOSyntax.this.F }

  def F: MonadControlIO[F]
  ////

  ////
}
