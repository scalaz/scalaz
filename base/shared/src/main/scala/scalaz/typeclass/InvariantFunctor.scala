package scalaz
package typeclass

trait InvariantFunctor[F[_]] {
  def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B]
}
