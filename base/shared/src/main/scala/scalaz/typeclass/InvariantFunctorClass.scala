package scalaz
package typeclass

trait InvariantFunctorClass[F[_]] {
  def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B]
}
