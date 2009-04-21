package scalaz

sealed trait ByteMultiplication {
  val value: Byte
}

object ByteMultiplication {
  def multiplication(n: Byte) = new ByteMultiplication {
    val value = n
  }

  implicit def ByteMultiplicationFrom(n: ByteMultiplication) = n.value
}
