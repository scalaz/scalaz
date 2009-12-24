package scalaz

sealed trait ByteMultiplication extends NewType[Byte]

trait ByteMultiplications {
  def multiplication(n: Byte) = new ByteMultiplication {
    val value = n
  }
}
