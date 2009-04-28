package scalaz.test

sealed trait Gen[+A] {
  def apply(sz: Int)(implicit r: Rand): Option[A]
}

object Gen {
  def gen[A](f: (Int, Rand) => Option[A]) = new Gen[A] {
    def apply(sz: Int)(implicit r: Rand) = f(sz, r)
  }
}