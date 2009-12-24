package scalaz

trait LastOption[A] extends NewType[Option[A]]

trait LastOptions {
  implicit def LastOptionTo[A](a: Option[A]): LastOption[A] = new LastOption[A] {
    val value = a
  }
}
