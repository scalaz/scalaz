package scalaz

sealed trait ArrayByte extends PimpedType[Array[Byte]] {
  def decode(implicit c: CharSet) = new String(value, c.value)
}

trait ArrayBytes {
  implicit def ArrayByteTo(bs: Array[Byte]): ArrayByte = new ArrayByte {
    val value = bs
  }
}
