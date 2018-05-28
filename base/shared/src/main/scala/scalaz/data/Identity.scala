package scalaz
package data

import scala.AnyVal

final case class Identity[A](run: A) extends AnyVal
