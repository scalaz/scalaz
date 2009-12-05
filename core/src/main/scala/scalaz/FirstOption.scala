package scalaz

trait FirstOption[A] {
  val value : Option[A]
}

trait FirstOptions {
  implicit def FirstOptionTo[A](a: Option[A]): FirstOption[A] = new FirstOption[A] {
    val value = a
  }

  implicit def FirstOptionFrom[A](d: FirstOption[A]): Option[A] = d.value
}
