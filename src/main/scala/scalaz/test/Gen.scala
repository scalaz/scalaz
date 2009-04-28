package scalaz.test

sealed trait Gen[+A] {
  def apply(sz: Int)(implicit r: Rand): A
}

object Gen {
  def gen[A](f: (Int, Rand) => A) = new Gen[A] {
    def apply(sz: Int)(implicit r: Rand) = f(sz, r)
  }
}