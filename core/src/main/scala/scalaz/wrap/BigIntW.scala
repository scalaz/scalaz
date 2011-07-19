package scalaz
package wrap

sealed trait BigIntW {

  import newtypes._

  val value: BigInt

  def multiplication: BigIntMultiplication =
    ^*^.->^*^[BigInt, BigIntMultiplication](value)

  def ∏ : BigIntMultiplication =
    multiplication
}

object BigIntW extends BigIntWs

trait BigIntWs {
  implicit def BigIntTo(n: BigInt): BigIntW = new BigIntW {
    val value = n
  }
}