package scalaz

sealed trait Resource[A] {
  val close: A => Unit
}

object Resource extends Resources

trait Resources {
  def resource[A](c: A => Unit): Resource[A] = new Resource[A] {
    val close = c
  }
}
