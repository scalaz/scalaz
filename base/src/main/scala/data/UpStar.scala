package scalaz
package data

final case class UpStar[F[_], A, B](run: A => F[B]) extends AnyVal

object UpStar extends UpStarInstances
