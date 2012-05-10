package scalaz
package syntax

import Id._

trait KleisliIdOps[A] extends Ops[A] {
  /** Lift the value into a Kleisli */
  def liftKleisliId[R]: Kleisli[Id, R, A] = Kleisli[Id, R, A](_ => self)

  /** Lift the value into a Reader. Alias for liftKleisliId */
  def liftReader[R]: Reader[R, A] = liftKleisliId
}

trait KleisliFAOps[F[+_], A] extends Ops[F[A]] {
  /** Lift the computation into a Kleisli */
  def liftKleisli[R]: Kleisli[F, R, A] = Kleisli[F, R, A](_ => self)

  /** Lift the computation into a ReaderT. Alias for liftKleisli */
  def liftReaderT[R]: ReaderT[F, R, A] = liftKleisli
}

trait ToKleisliOps {
  implicit def ToKleisliIdOps[A](a: A) = new KleisliIdOps[A]{ def self = a }
  implicit def ToKleisliFAOps[F[+_], A](fa: F[A]) = new KleisliFAOps[F, A] { def self = fa }
}
