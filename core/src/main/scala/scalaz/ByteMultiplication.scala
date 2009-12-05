package scalaz

sealed trait ByteMultiplication {
  val value: Byte
}

trait ByteMultiplications {
  def multiplication(n: Byte) = new ByteMultiplication {
    val value = n
  }

  implicit def ByteMultiplicationFrom(n: ByteMultiplication): Byte = n.value
}
