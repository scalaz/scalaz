package scalaz
package syntax
package effect

import scalaz.effect.LiftIO

/** Wraps a value `self` and provides methods related to `LiftIO` */
final class LiftIOOps[F[_],A] private[syntax](val self: F[A])(implicit val F: LiftIO[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToLiftIOOpsU[TC[F[_]] <: LiftIO[F]] {
  implicit def ToLiftIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): LiftIOOps[F0.M, F0.A] =
    new LiftIOOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToLiftIOOps0[TC[F[_]] <: LiftIO[F]] extends ToLiftIOOpsU[TC] {
  implicit def ToLiftIOOps[F[_],A](v: F[A])(implicit F0: TC[F]): LiftIOOps[F, A] =
    new LiftIOOps[F, A](v)

  ////

  ////
}

trait ToLiftIOOps[TC[F[_]] <: LiftIO[F]] extends ToLiftIOOps0[TC]

trait LiftIOSyntax[F[_]]  {
  implicit def ToLiftIOOps[A](v: F[A]): LiftIOOps[F, A] = new LiftIOOps[F,A](v)(using LiftIOSyntax.this.F)

  def F: LiftIO[F]
  ////

  ////
}
