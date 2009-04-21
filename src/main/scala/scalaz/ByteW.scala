package scalaz

sealed trait ByteW {
  val value: Byte
}

object ByteW {
  implicit def ByteTo(n: Byte): ByteW = new ByteW {
    val value = n
  }

  implicit def ByteFrom(n: ByteW) = n.value
}
