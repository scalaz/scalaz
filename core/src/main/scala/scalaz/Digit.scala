package scalaz

/**An algebraic data type representing the digits 0 - 9 */
sealed trait Digit {
  val toInt: Int

  def toLong: Long = toInt.toLong

  def toChar: Char = (toLong + 48).toChar
}

object Digit extends DigitFunctions with DigitInstances {

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

trait DigitFunctions {

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

  def longDigits[F[_]](digits: F[Digit])(implicit F: Foldable[F]): Long =
    F.foldLeft(digits, 0L)((n, a) => n * 10L + (a: Digit))

  def digits[F[_]](cs: F[Char])(implicit F: Functor[F]): OptionT[F, Digit] =
    OptionT(F.map(cs)(digitFromChar))

  def digitsOr[F[_]](chars: F[Char], d: => Digit)(implicit F: Functor[F]): F[Digit] =
    F.map(chars)(a => digitFromChar(a) getOrElse d)

  def digitsCollapse[F[_]](chars: F[Char])(implicit F: MonadPlus[F]): F[Digit] =
    F.bind(chars)(a => Digit.digitFromChar(a) match {
      case None    => F.empty[Digit]
      case Some(d) => F.point(d)
    })

  def traverseDigits[F[_]](chars: F[Char])(implicit F: Traverse[F]): Option[F[Digit]] = {
    import std.option._
    F.sequence(digits(chars).run)
  }

  def traverseDigitsOr[F[_]](chars: F[Char], d: => F[Digit])(implicit F: Traverse[F]): F[Digit] =
    traverseDigits(chars) getOrElse d
}

trait DigitInstances {
  implicit def digitInstances: Enum[Digit] with Show[Digit] with Monoid[Digit] = new Enum[Digit] with Show[Digit] with Monoid[Digit] {

    import std.anyVal._

    def succ(d: Digit) = d match {
      case Digit._0 => Digit._1
      case Digit._1 => Digit._2
      case Digit._2 => Digit._3
      case Digit._3 => Digit._4
      case Digit._4 => Digit._5
      case Digit._5 => Digit._6
      case Digit._6 => Digit._7
      case Digit._7 => Digit._8
      case Digit._8 => Digit._9
      case Digit._9 => Digit._0
    }

    def pred(d: Digit) = d match {
      case Digit._0 => Digit._9
      case Digit._1 => Digit._0
      case Digit._2 => Digit._1
      case Digit._3 => Digit._2
      case Digit._4 => Digit._3
      case Digit._5 => Digit._4
      case Digit._6 => Digit._5
      case Digit._7 => Digit._6
      case Digit._8 => Digit._7
      case Digit._9 => Digit._8
    }

    override def succn(n: Int, a: Digit) =
      super.succn(n % 10, a)

    override def predn(n: Int, a: Digit) =
      super.predn(n % 10, a)

    override def min = Some(Digit._0)

    override def max = Some(Digit._9)

    override def shows(f: Digit) = f.toChar.toString
    def order(x: Digit, y: Digit): Ordering = Order[Int].order(x.toInt, y.toInt)
    override def equal(x: Digit, y: Digit): Boolean = x == y
    def append(f1: Digit, f2: => Digit): Digit = Digit.mod10Digit(f2.toInt + f2.toInt)
    def zero: Digit = Digit._0
  }
}
