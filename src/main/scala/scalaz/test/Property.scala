package scalaz.test

sealed trait Property {
  def apply(sz: Int, r: Rand): Result
}

object Property {
  def property(f: (Int, Rand) => Result) = new Property {
    def apply(sz: Int, r: Rand) = f(sz, r)
  }
}