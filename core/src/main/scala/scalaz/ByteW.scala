package scalaz

sealed trait ByteW {
  val value: Byte

  import Scalaz._
  
  def ∏ = multiplication(value)
}

trait Bytes {
  implicit def ByteTo(n: Byte): ByteW = new ByteW {
    val value = n
  }

  implicit def ByteFrom(n: ByteW): Byte = n.value
}
