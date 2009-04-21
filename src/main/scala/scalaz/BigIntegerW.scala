package scalaz

import java.math.BigInteger

sealed trait BigIntegerW {
  val value: BigInteger

  def |*| = BigIntegerMultiplication.multiplication(value)
}

object BigIntegerW {
  implicit def BigIntegerTo(n: BigInteger) = new BigIntegerW {
    val value = n
  }

  implicit def BigIntegerFrom(n: BigIntegerW) = n.value
}
