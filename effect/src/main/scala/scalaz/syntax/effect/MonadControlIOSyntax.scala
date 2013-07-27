package scalaz
package syntax
package effect

import scalaz.effect.MonadControlIO

/** Wraps a value `self` and provides methods related to `MonadControlIO` */
final class MonadControlIOOps[F[_],A] private[syntax](val self: F[A])(implicit val F: MonadControlIO[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToMonadControlIOOps0 {
  implicit def ToMonadControlIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadControlIO, FA]) =
    new MonadControlIOOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToMonadControlIOOps extends ToMonadControlIOOps0 with ToLiftControlIOOps with ToMonadOps {
  implicit def ToMonadControlIOOps[F[_],A](v: F[A])(implicit F0: MonadControlIO[F]) =
    new MonadControlIOOps[F,A](v)

  ////

  ////
}

trait MonadControlIOSyntax[F[_]] extends LiftControlIOSyntax[F] with MonadSyntax[F] {
  implicit def ToMonadControlIOOps[A](v: F[A]): MonadControlIOOps[F, A] = new MonadControlIOOps[F,A](v)(MonadControlIOSyntax.this.F)

  def F: MonadControlIO[F]
  ////

  ////
}
