package scalaz

trait Each[E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
}

object Each {
  implicit val IdentityEach = new Each[Identity] {
    def each[A](e: Identity[A], f: A => Unit) = f(e.value)
  }
}
