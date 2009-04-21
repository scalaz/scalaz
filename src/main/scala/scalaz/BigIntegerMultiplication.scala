package scalaz

import java.math.BigInteger

sealed trait BigIntegerMultiplication {
  val value: BigInteger
}

object BigIntegerMultiplication {
  def multiplication(n: BigInteger) = new BigIntegerMultiplication {
    val value = n
  }

  implicit def BigIntegerMultiplicationFrom(n: BigIntMultiplication) = n.value
}
 