package scalaz
package wrap

import java.math.BigInteger

sealed trait BigIntegerW {

  import newtypes._

  val value: BigInteger

  def multiplication: BigIntegerMultiplication =
    ^*^.->^*^[BigInteger, BigIntegerMultiplication](value)

  def ∏ : BigIntegerMultiplication =
    multiplication
}

object BigIntegerW extends BigIntegerWs

trait BigIntegerWs {
  implicit def BigIntegerTo(n: BigInteger): BigIntegerW = new BigIntegerW {
    val value = n
  }
}