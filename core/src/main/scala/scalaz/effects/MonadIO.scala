package scalaz
package effects

import scalaz._

/** Classifies IO-like monads. */
sealed abstract class MonadIO[M[_]](implicit val m: Monad[M]) {
  def bracket[A, B, C](before: M[A], after: A => M[B], during: A => M[C]): M[C]
  def snag[A](m: M[A], onError: Throwable => M[A]): M[A]
  def liftIO[A](a: IO[A]): M[A]
}

