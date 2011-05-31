package scalaz


trait ^*^[T, R] {
  val unpack: T => R
  val pack: R => T
}

object ^*^ extends ^^*^^

trait ^^*^^ {
  def ^*^[T, R](u: T => R, p: R => T): ^*^[T, R] = new ^*^[T, R] {
    val unpack = u
    val pack = p
  }

  def ->^*^[R, T](r: R)(implicit p: ^*^[T, R]): T =
    p.pack(r)

  def <-^*^[T, R](t: T)(implicit u: ^*^[T, R]): R =
    u.unpack(t)
}

trait ^**^[F[_], G[_]] {
  def unpack[A]: F[A] => G[A]

  def pack[A]: G[A] => F[A]
}
