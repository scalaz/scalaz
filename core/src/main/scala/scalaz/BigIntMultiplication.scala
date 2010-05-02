package scalaz

sealed trait BigIntMultiplication extends NewType[BigInt]

trait BigIntMultiplications {
  def multiplication(n: BigInt): BigIntMultiplication = new BigIntMultiplication {
    val value = n
  }
}
