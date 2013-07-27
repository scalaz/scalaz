package scalaz
package syntax
package effect

import scalaz.effect.LiftControlIO

/** Wraps a value `self` and provides methods related to `LiftControlIO` */
final class LiftControlIOOps[F[_],A] private[syntax](val self: F[A])(implicit val F: LiftControlIO[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToLiftControlIOOps0 {
  implicit def ToLiftControlIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[LiftControlIO, FA]) =
    new LiftControlIOOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToLiftControlIOOps extends ToLiftControlIOOps0 {
  implicit def ToLiftControlIOOps[F[_],A](v: F[A])(implicit F0: LiftControlIO[F]) =
    new LiftControlIOOps[F,A](v)

  ////

  ////
}

trait LiftControlIOSyntax[F[_]]  {
  implicit def ToLiftControlIOOps[A](v: F[A]): LiftControlIOOps[F, A] = new LiftControlIOOps[F,A](v)(LiftControlIOSyntax.this.F)

  def F: LiftControlIO[F]
  ////

  ////
}
