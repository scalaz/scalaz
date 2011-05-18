package scalaz
package wrap

import java.math.BigInteger

sealed trait BigIntegerW {

  import newtypes._

  val value: BigInteger

  def multiplication: BigIntegerMultiplication =
    Pack.pack[BigInteger, BigIntegerMultiplication](value)

  def ∏ : BigIntegerMultiplication =
    multiplication
}

trait BigIntegerWs {
  implicit def BigIntegerTo(n: BigInteger): BigIntegerW = new BigIntegerW {
    val value = n
  }
}