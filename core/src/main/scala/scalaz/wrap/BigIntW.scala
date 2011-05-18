package scalaz

sealed trait BigIntW extends PimpedType[BigInt] {
  import Scalaz._
  
  def ∏ : BigIntMultiplication = multiplication(value)
}

trait BigInts {
  implicit def BigIntTo(n: BigInt): BigIntW = new BigIntW {
    val value = n
  }
}
