package scalaz
package syntax

import Id._

final class KleisliIdOps[A](val self: A) extends Super {
  /** Lift the value into a Kleisli */
  def liftKleisliId[R]: Kleisli[Id, R, A] = Kleisli[Id, R, A](_ => self)

  /** Lift the value into a Reader. Alias for liftKleisliId */
  def liftReader[R]: Reader[R, A] = liftKleisliId
}

final class KleisliFAOps[F[_], A](val self: F[A]) extends Super {
  /** Lift the computation into a Kleisli */
  def liftKleisli[R]: Kleisli[F, R, A] = Kleisli[F, R, A](_ => self)

  /** Lift the computation into a ReaderT. Alias for liftKleisli */
  def liftReaderT[R]: ReaderT[F, R, A] = liftKleisli
}

trait ToKleisliOps0 {
  implicit def ToKleisliOpsUnapply[FA](v: FA)(implicit F0: Unapply[Monad, FA]): KleisliFAOps[F0.M, F0.A] =
    new KleisliFAOps(F0(v))
}

trait ToKleisliOps extends ToKleisliOps0 {
  implicit def ToKleisliIdOps[A](a: A) = new KleisliIdOps(a)
  implicit def ToKleisliFAOps[F[_], A](fa: F[A]) = new KleisliFAOps(fa)
}
