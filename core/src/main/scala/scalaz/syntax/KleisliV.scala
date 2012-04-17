package scalaz
package syntax

trait KleisliIdV[A] extends SyntaxV[A] {
  /** Lift the value into a Kleisli */
  def liftKleisliId[R]: Kleisli[Id, R, A] = Kleisli[Id, R, A](_ => self)

  /** Lift the value into a Reader. Alias for liftKleisliId */
  def liftReader[R]: Reader[R, A] = liftKleisliId
}

trait KleisliFAV[F[_], A] extends SyntaxV[F[A]] {
  /** Lift the computation into a Kleisli */
  def liftKleisli[R]: Kleisli[F, R, A] = Kleisli[F, R, A](_ => self)

  /** Lift the computation into a ReaderT. Alias for liftKleisli */
  def liftReaderT[R]: ReaderT[F, R, A] = liftKleisli
}

trait ToKleisliV {
  implicit def ToKleisliIdV[A](a: A) = new KleisliIdV[A]{ def self = a }
  implicit def ToKleisliFAV[F[_], A](fa: F[A]) = new KleisliFAV[F, A] { def self = fa }
}
