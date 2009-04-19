package scalaz

sealed trait ArrayByte {
  val value: Array[Byte]

  def decode(implicit c: CharSet) = new String(value, c.value)
}

object ArrayByte {
  implicit def ArrayByteTo(bs: Array[Byte]) = new ArrayByte {
    val value = bs
  }

  implicit def ArrayByteFrom(bs: ArrayByte) = bs.value
}
