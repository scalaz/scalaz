package scalaz

sealed trait StreamW[A] {
  val value: Stream[A]

  def string(f: A => Char) = value map f mkString

  def stringj(f: A => Stream[Char]) = value flatMap f mkString

  def |!| = ZipStream.zip(value)

  import StreamW._

  def merge(s: Stream[A]): Stream[A] =
    if (value.isEmpty) Stream.empty
    else Stream.cons(value.head, s.merge(value.tail))
}

object StreamW {
  implicit def StreamTo[A](as: Stream[A]): StreamW[A] = new StreamW[A] {
    val value = as
  }

  implicit def StreamFrom[A](as: StreamW[A]): Stream[A] = as.value
}
