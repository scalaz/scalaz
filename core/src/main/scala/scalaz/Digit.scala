package scalaz

sealed trait Digit {
  val toInt: Int

  def toLong: Long = toInt.toLong

  def toChar: Char = (toLong + 48).toChar
}

object Digit extends Digits

trait Digits {

  import Digit._

  val digits: List[Digit] = List(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9)

  implicit def DigitLong(d: Digit): Long = d.toLong

  def digitFromChar(c: Char): Option[Digit] =
    digits.find(_.toChar == c)

  def digitFromInt(i: Int): Option[Digit] =
    digits.find(_.toInt == i)

  def digitFromLong(i: Long): Option[Digit] =
    digits.find(_.toLong == i)

  def mod10Digit(i: Int): Digit =
    i match {
      case 0 => _0
      case 1 => _1
      case 2 => _2
      case 3 => _3
      case 4 => _4
      case 5 => _5
      case 6 => _6
      case 7 => _7
      case 8 => _8
      case 9 => _9
      case _ => mod10Digit(scala.math.abs(i) % 10)
    }

  object Digit {

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

  }

}
