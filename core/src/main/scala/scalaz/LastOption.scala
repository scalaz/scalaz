package scalaz

trait LastOption[A] {
  val value : Option[A]
}

trait LastOptions {
  implicit def LastOptionTo[A](a: Option[A]): LastOption[A] = new LastOption[A] {
    val value = a
  }

  implicit def LastOptionFrom[A](d: LastOption[A]): Option[A] = d.value
}
