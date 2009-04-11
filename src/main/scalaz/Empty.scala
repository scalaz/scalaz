package scalaz

trait Empty[E[_]] {
  def empty[A]: E[A]
}

object Empty {
  implicit val OptionEmpty = new Empty[Option] {
    def empty[A] = None
  }
}
