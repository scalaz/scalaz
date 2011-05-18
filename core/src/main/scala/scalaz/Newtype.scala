package scalaz


trait Newtype[T, R] {
  val unpack: Unpack[T, R]
  val pack: Pack[T, R]

  def unpk: T => R =
    unpack.unpack

  def pk: R => T =
    pack.pack
}

object Newtype extends Newtypes

trait Newtypes {
  def newtype[T, R](implicit u: Unpack[T, R], p: Pack[T, R]): Newtype[T, R] = new Newtype[T, R] {
    val unpack = u
    val pack = p
  }
}
