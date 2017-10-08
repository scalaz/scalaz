package scalaz
package syntax
package effect

import scalaz.effect.MonadIO

/** Wraps a value `self` and provides methods related to `MonadIO` */
final class MonadIOOps[F[_],A] private[syntax](val self: F[A])(implicit val F: MonadIO[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToMonadIOOpsU[TC[F[_]] <: MonadIO[F]] {
  implicit def ToMonadIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new MonadIOOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToMonadIOOps0[TC[F[_]] <: MonadIO[F]] extends ToMonadIOOpsU[TC] {
  implicit def ToMonadIOOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new MonadIOOps[F,A](v)

  ////

  ////
}

trait ToMonadIOOps[TC[F[_]] <: MonadIO[F]] extends ToMonadIOOps0[TC] with ToLiftIOOps[TC] with ToMonadOps[TC]

trait MonadIOSyntax[F[_]] extends LiftIOSyntax[F] with MonadSyntax[F] {
  implicit def ToMonadIOOps[A](v: F[A]): MonadIOOps[F, A] = new MonadIOOps[F,A](v)(MonadIOSyntax.this.F)

  def F: MonadIO[F]
  ////

  ////
}
