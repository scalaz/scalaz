package scalaz

sealed trait Digit {
  val toInt: Int
  val toLong = toInt.toLong

  def toChar = (toLong + 48).toChar

  override def equals(o: Any) = o.isInstanceOf[Digit] && o.asInstanceOf[Digit].toInt == toInt

  override def hashCode = toInt

  override def toString = toInt.toString
}
final case object _0 extends Digit {
  override val toInt = 0
}
final case object _1 extends Digit {
  override val toInt = 1
}
final case object _2 extends Digit {
  override val toInt = 2
}
final case object _3 extends Digit {
  override val toInt = 3
}
final case object _4 extends Digit {
  override val toInt = 4
}
final case object _5 extends Digit {
  override val toInt = 5
}
final case object _6 extends Digit {
  override val toInt = 6
}
final case object _7 extends Digit {
  override val toInt = 7
}
final case object _8 extends Digit {
  override val toInt = 8
}
final case object _9 extends Digit {
  override val toInt = 9
}

object Digit {
  val digits = List(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9)

  implicit def DigitLong(d: Digit) = d.toLong

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

  /*
  def longDigits[T[_]](n: Long)(implicit f: FoldLeft[T], m: MonadEmptyPlus[T]): T[Digit] =
    if(n == 0) m.pure(_0) else foldleft[T](unfold[T]((b: Long) => if(b == 0) None else Some (b % 10L: Digit, b / 10L), n)).rev[T]

  def digitsLong[T[_]](ds: T[Digit])(implicit f: FoldLeft[T]) =
    f.foldLeft[Long, Digit](ds, 0L, (a, b) => a * 10L + b)

  def charDigits(c: List[Char]) =
    charDigits[List](c).foldRight[Option[List[Digit]]](Some(Nil))((a, b) => for(j <- a; k <- b) yield j :: k)

  def digitsValue(c: List[Char]) = charDigits(c) map (ds => digitsLong[List](ds))
  */
}
