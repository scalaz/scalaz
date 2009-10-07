package scalaz

sealed trait BigIntW {
  val value: BigInt

  def |*| = BigIntMultiplication.multiplication(value)
}

object BigIntW {
  implicit def BigIntTo(n: BigInt) = new BigIntW {
    val value = n
  }

  implicit def BigIntFrom(n: BigIntW) = n.value
}