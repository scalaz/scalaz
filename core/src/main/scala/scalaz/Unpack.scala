package scalaz

trait Unpack[T, R] {
  val unpack: T => R
}

object Unpack extends Unpacks

trait Unpacks {
  def unpack[T, R](t: T)(implicit u: Unpack[T, R]): R =
    u.unpack(t)
}
