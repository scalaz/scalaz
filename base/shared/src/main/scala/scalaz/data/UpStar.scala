package scalaz
package data

import scala.AnyVal

final case class UpStar[F[_], A, B](run: A => F[B]) extends AnyVal
