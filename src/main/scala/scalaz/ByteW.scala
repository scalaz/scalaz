package scalaz

sealed trait ByteW {
  val value: Byte

  def |*| = ByteMultiplication.multiplication(value)  
}

object ByteW {
  implicit def ByteTo(n: Byte): ByteW = new ByteW {
    val value = n
  }

  implicit def ByteFrom(n: ByteW) = n.value
}
