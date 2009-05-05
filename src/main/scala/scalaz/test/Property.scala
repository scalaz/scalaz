package scalaz.test

sealed trait Property {
  def apply(sz: Int)(implicit r: Rand): Result

  def gen = Gen.gen((sz, r) => Some(apply(sz)(r)))
}

object Property {
  def property(f: (Int, Rand) => Result) = new Property {
    def apply(sz: Int)(implicit r: Rand) = f(sz, r)
  }
}