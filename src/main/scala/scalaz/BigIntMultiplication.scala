package scalaz

sealed trait BigIntMultiplication {
  val value: BigInt
}

object BigIntMultiplication {
  def multiplication(n: BigInt) = new BigIntMultiplication {
    val value = n
  }

  implicit def BigIntMultiplicationFrom(n: BigIntMultiplication) = n.value
}
