package scalaz

sealed trait ZipStream[+A] {
  val value: Stream[A]
}

object ZipStream {
  def zip[A](s: Stream[A]) = new ZipStream[A] {
    val value = s
  }

  implicit def ZipStreamFrom[A](z: ZipStream[A]) = z.value
}
