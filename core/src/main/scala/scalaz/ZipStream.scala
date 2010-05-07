package scalaz

trait ZipStream[+A] {
  val value: Stream[A]
}

trait ZipStreams {
  def zip[A](s: Stream[A]): ZipStream[A] = new ZipStream[A] {
    val value = s
  }

  def emptyZipStream : ZipStream[Nothing] = zip(Stream.Empty)

  implicit def ZipStreamFrom[A](z: ZipStream[A]): Stream[A] = z.value
}
