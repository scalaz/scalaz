package scalaz
package data

case class Identity[A](run: A) extends AnyVal

object Identity extends IdentityTypes with IdentityInstances
