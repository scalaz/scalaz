package scalaz

sealed trait BigIntMultiplication extends NewType[BigInt]

trait BigIntMultiplications {
  def multiplication(n: BigInt) = new BigIntMultiplication {
    val value = n
  }
}
