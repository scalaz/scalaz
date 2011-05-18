package scalaz

trait Pack[T, R] {
  val pack: R => T
}

object Pack extends Packs

trait Packs {
  def pack[R, T](r: R)(implicit p: Pack[T, R]): T =
    p.pack(r)
}
