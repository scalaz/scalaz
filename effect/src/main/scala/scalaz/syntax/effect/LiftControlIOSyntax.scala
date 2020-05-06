package scalaz
package syntax
package effect

import scalaz.effect.LiftControlIO

/** Wraps a value `self` and provides methods related to `LiftControlIO` */
final class LiftControlIOOps[F[_],A] private[syntax](val self: F[A])(implicit val F: LiftControlIO[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToLiftControlIOOpsU[TC[F[_]] <: LiftControlIO[F]] {
  implicit def ToLiftControlIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): LiftControlIOOps[F0.M, F0.A] =
    new LiftControlIOOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToLiftControlIOOps0[TC[F[_]] <: LiftControlIO[F]] extends ToLiftControlIOOpsU[TC] {
  implicit def ToLiftControlIOOps[F[_],A](v: F[A])(implicit F0: TC[F]): LiftControlIOOps[F, A] =
    new LiftControlIOOps[F, A](v)

  ////

  ////
}

trait ToLiftControlIOOps[TC[F[_]] <: LiftControlIO[F]] extends ToLiftControlIOOps0[TC]

trait LiftControlIOSyntax[F[_]]  {
  implicit def ToLiftControlIOOps[A](v: F[A]): LiftControlIOOps[F, A] = new LiftControlIOOps[F,A](v)(LiftControlIOSyntax.this.F)

  def F: LiftControlIO[F]
  ////

  ////
}
