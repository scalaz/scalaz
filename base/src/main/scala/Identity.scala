package scalaz

case class Identity[A](run: A) extends AnyVal

object Identity {
  type Id[X] = X
}
