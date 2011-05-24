package scalaz


sealed trait Newtype[T, R] {
  val unpack: T => R
  val pack: R => T
}

object Newtype extends Newtypes

trait Newtypes {
  def newtype[T, R](u: T => R, p: R => T): Newtype[T, R] = new Newtype[T, R] {
    val unpack = u
    val pack = p
  }

  def pack[R, T](r: R)(implicit p: Newtype[T, R]): T =
    p.pack(r)

  def unpack[T, R](t: T)(implicit u: Newtype[T, R]): R =
    u.unpack(t)
}
