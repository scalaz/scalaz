package scalaz

import java.math.BigInteger

sealed trait BigIntegerMultiplication extends NewType[BigInteger]

trait BigIntegerMultiplications {
  def multiplication(n: BigInteger) = new BigIntegerMultiplication {
    val value = n
  }
}
