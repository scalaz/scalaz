package scalaz

import java.math.BigInteger

sealed trait BigIntegerMultiplication {
  val value: BigInteger
}

trait BigIntegerMultiplications {
  def multiplication(n: BigInteger) = new BigIntegerMultiplication {
    val value = n
  }

  implicit def BigIntegerMultiplicationFrom(n: BigIntegerMultiplication): BigInteger = n.value
}
