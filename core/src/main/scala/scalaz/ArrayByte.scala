package scalaz

sealed trait ArrayByte {
  val value: Array[Byte]

  def decode(implicit c: CharSet) = new String(value, c.value)
}

trait ArrayBytes {
  implicit def ArrayByteTo(bs: Array[Byte]): ArrayByte = new ArrayByte {
    val value = bs
  }

  implicit def ArrayByteFrom(bs: ArrayByte): Array[Byte] = bs.value
}
