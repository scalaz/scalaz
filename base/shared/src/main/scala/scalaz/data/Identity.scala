package scalaz
package data

final case class Identity[A](run: A) extends AnyVal
