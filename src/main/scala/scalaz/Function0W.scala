package scalaz

sealed trait Function0W[T] {
  val k: () => T

  def throws = try { Right(k()) } catch { case e => Left(e) }

  def pure[P[_]](implicit p: Pure[P]) = p pure k()
}

object Function0W {
  implicit def Function0To[T](f: () => T) = new Function0W[T] {
    val k = f
  }

  implicit def Function0From[T](f: Function0W[T]) = f.k
}
