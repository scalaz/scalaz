package scalaz
package syntax

import Id._

final class KleisliIdOps[A](private val self: A) extends AnyVal {
  /** Lift the value into a Kleisli */
  def liftKleisliId[R]: Kleisli[Id, R, A] = Kleisli[Id, R, A](_ => self)

  /** Lift the value into a Reader. Alias for liftKleisliId */
  def liftReader[R]: Reader[R, A] = liftKleisliId
}

final class KleisliFAOps[F[_], A](private val self: F[A]) extends AnyVal {
  /** Lift the computation into a Kleisli */
  def liftKleisli[R]: Kleisli[F, R, A] = Kleisli[F, R, A](_ => self)

  /** Lift the computation into a ReaderT. Alias for liftKleisli */
  def liftReaderT[R]: ReaderT[R, F, A] = liftKleisli
}

sealed trait ToKleisliOps0 {
  implicit def ToKleisliOpsUnapply[FA](v: FA)(implicit F0: Unapply[Monad, FA]): KleisliFAOps[F0.M, F0.A] =
    new KleisliFAOps(F0(v))
}

trait ToKleisliOps extends ToKleisliOps0 {
  implicit def ToKleisliIdOps[A](a: A): KleisliIdOps[A] = new KleisliIdOps(a)
  implicit def ToKleisliFAOps[F[_], A](fa: F[A]): KleisliFAOps[F, A] = new KleisliFAOps(fa)
}
