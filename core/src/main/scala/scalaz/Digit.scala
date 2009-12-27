package scalaz


sealed trait Digit {
  val toInt: Int
  def toLong = toInt.toLong

  def toChar = (toLong + 48).toChar

  override def equals(o: Any) = o ne null && o.isInstanceOf[Digit] && o.asInstanceOf[Digit].toInt == toInt

  override def hashCode = toInt.hashCode

  override def toString = toInt.toString
}
case object _0 extends Digit {
  override val toInt = 0
}
case object _1 extends Digit {
  override val toInt = 1
}
case object _2 extends Digit {
  override val toInt = 2
}
case object _3 extends Digit {
  override val toInt = 3
}
case object _4 extends Digit {
  override val toInt = 4
}
case object _5 extends Digit {
  override val toInt = 5
}
case object _6 extends Digit {
  override val toInt = 6
}
case object _7 extends Digit {
  override val toInt = 7
}
case object _8 extends Digit {
  override val toInt = 8
}
case object _9 extends Digit {
  override val toInt = 9
}

trait Digits {
  val digits = List(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9)

  implicit def DigitLong(d: Digit): Long = d.toLong

  implicit def LongDigit(n: Long): Digit = n match {
    case 0L => _0
    case 1L => _1
    case 2L => _2
    case 3L => _3
    case 4L => _4
    case 5L => _5
    case 6L => _6
    case 7L => _7
    case 8L => _8
    case 9L => _9
    case _ => Math.abs(n) % 10L
  }
}
