package scalaz
package wrap

sealed trait ByteW {

  import newtypes._

  val value: Byte

  def multiplication: ByteMultiplication =
    Pack.pack[Byte, ByteMultiplication](value)

  def ∏ : ByteMultiplication =
    multiplication
}

object ByteW extends ByteWs

trait ByteWs {
  implicit def ByteTo(n: Byte): ByteW = new ByteW {
    val value = n
  }
}