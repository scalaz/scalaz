package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monad` */
final class MonadOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Monad[F]) extends Ops[F[A]] {
  ////

  def liftM[G[_[_], _]](implicit G: MonadTrans[G]): G[F, A] = G.liftM(self)

  def whileM[G[_]](p: F[Boolean])(implicit G: MonadPlus[G]): F[G[A]] = F.whileM(p, self)

  def whileM_(p: F[Boolean]): F[Unit] = F.whileM_(p, self)

  def untilM[G[_]](p: => F[Boolean])(implicit G: MonadPlus[G]): F[G[A]] = F.untilM(self, p)

  def untilM_(p: => F[Boolean]): F[Unit] = F.untilM_(self, p)

  def iterateWhile(p: A => Boolean): F[A] = F.iterateWhile(self)(p)

  def iterateUntil(p: A => Boolean): F[A] = F.iterateUntil(self)(p)

  ////
}

sealed trait ToMonadOpsU[TC[F[_]] <: Monad[F]] {
  implicit def ToMonadOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new MonadOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToMonadOps0[TC[F[_]] <: Monad[F]] extends ToMonadOpsU[TC] {
  implicit def ToMonadOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new MonadOps[F,A](v)

  ////

  ////
}

trait ToMonadOps[TC[F[_]] <: Monad[F]] extends ToMonadOps0[TC] with ToApplicativeOps[TC] with ToBindOps[TC]

trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F] {
  implicit def ToMonadOps[A](v: F[A]): MonadOps[F, A] = new MonadOps[F,A](v)(MonadSyntax.this.F)

  def F: Monad[F]
  ////

  ////
}
