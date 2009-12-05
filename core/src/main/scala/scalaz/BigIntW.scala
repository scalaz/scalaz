package scalaz

sealed trait BigIntW {
  val value: BigInt

  import Scalaz._
  
  def ∏ = multiplication(value)
}

trait BigInts {
  implicit def BigIntTo(n: BigInt): BigIntW = new BigIntW {
    val value = n
  }

  implicit def BigIntFrom(n: BigIntW): BigInt = n.value
}
