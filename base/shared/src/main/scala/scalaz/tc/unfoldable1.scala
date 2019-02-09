package scalaz
package tc

import scalaz.data.Maybe

trait Unfoldable1Class[F[_]] {
  def unfoldRight1[A, B](f: B => (A, Maybe[B]))(b: B): F[A]
  //  def fromNonEmptyList[A](as: NonEmptyList[A]): F[A]
}
