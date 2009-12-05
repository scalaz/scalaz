package scalaz

sealed trait BigIntMultiplication {
  val value: BigInt
}

trait BigIntMultiplications {
  def multiplication(n: BigInt) = new BigIntMultiplication {
    val value = n
  }

  implicit def BigIntMultiplicationFrom(n: BigIntMultiplication): BigInt = n.value
}
