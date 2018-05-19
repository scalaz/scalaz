package scalaz
package std

import _root_.java.math.{ BigDecimal, BigInteger }

import scala.reflect.ClassTag
import scala.util.control.NonFatal

trait StringInstances {
  implicit object stringInstance extends Monoid[String] with Show[String] with Equal[String] with Order[String] with IsEmpty[λ[α => String]] {
    type SA[α] = String
    def append(f1: String, f2: => String) = f1 + f2
    def zero: String = ""
    override def show(f: String): Cord = Cord("\"") ++ (Cord(f) ++ Cord("\""))
    override def shows(f: String): String = '"' + f + '"'
    def order(x: String, y: String) = Ordering.fromInt(x.compareTo(y))
    override def equal(x: String, y: String) = x == y
    override def equalIsNatural: Boolean = true
    def empty[A] = zero
    def plus[A](f1: SA[A], f2: => SA[A]) = f1 + f2
    def isEmpty[A](s: SA[A]) = s == ""
  }
}

trait StringFunctions {
  private[this] implicit val instance = new StringInstances {}.stringInstance

  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(s:String, n: Long): String = if(n == 1L) s else
  if((s endsWith "y") && (List("ay", "ey","iy", "oy", "uy") forall (!s.endsWith(_)))) s.take(s.length - 1) + "ies"
  else s + "s"

  /**
   * Constructs a non-empty list with the value if it is not empty, otherwise, throws an error.
   */
  def charsNel(s:String) : Option[NonEmptyList[Char]] = list.toNel(s.toList)

  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def charsNel(s:String, e: => NonEmptyList[Char]) : NonEmptyList[Char] = charsNel(s) getOrElse e

  // Parsing functions.
  private def intChars(s: String): Boolean = s.length == 1 && s.charAt(0).isDigit || ((s.length > 1) && s.stripPrefix("-").forall(_.isDigit))

  private def asNumber[T](f: String => T, lowerBound: T, upperBound: T, s: String)(implicit t: ClassTag[T]): Validation[String, T] =
    try {
      Success(f(s))
    } catch {
      case _: NumberFormatException if intChars(s) => Failure(s"${s} is outside of range for ${t} (${lowerBound} - ${upperBound})")
      case _: NumberFormatException => Failure(s"${s} does not represent a valid ${t}")
      case NonFatal(e) => Failure(e.getMessage)
    }

  def parseLong(s: String): Validation[String, Long] = asNumber(_.toLong, Long.MinValue, Long.MaxValue, s)
  def parseInt(s: String): Validation[String, Int] = asNumber(_.toInt, Int.MinValue, Int.MaxValue, s)
  def parseByte(s: String): Validation[String, Byte] = asNumber(_.toByte, Byte.MinValue, Byte.MaxValue, s)
  def parseShort(s: String): Validation[String, Short] = asNumber(_.toShort, Short.MinValue, Short.MaxValue, s)

  def parseDouble(s: String): Validation[String, Double] =
    asNumber(_.toDouble, Double.MinValue, Double.MaxValue, s)
      .filter(_ != Double.NegativeInfinity)
      .filter(_ != Double.PositiveInfinity)
      .leftMap(e => if (e == instance.zero) s"${s} is outside of range for Double" else e)

  def parseFloat(s: String): Validation[String, Float] =
    asNumber(_.toFloat, Float.MinValue, Float.MaxValue, s)
      .filter(_ != Float.NegativeInfinity)
      .filter(_ != Float.PositiveInfinity)
      .leftMap(e => if (e == instance.zero) s"${s} is outside of range for Float" else e)

  def parseBigInt(s: String): Validation[String, BigInteger] =
    try {
      Success(new BigInteger(s))
    } catch {
      case _: NumberFormatException => Failure(s"${s} does not represent a valid BigInteger")
      case NonFatal(e) => Failure(e.getMessage)
    }

  def parseBigDecimal(s: String): Validation[String, BigDecimal] =
    try {
      Success(new BigDecimal(s))
    } catch {
      case _: NumberFormatException => Failure(s"${s} does not represent a valid BigDecimal")
      case NonFatal(e) => Failure(e.getMessage)
    }

  def parseBoolean(s: String): Validation[String, Boolean] =
    try {
      Success(s.toBoolean)
    } catch {
      case _: IllegalArgumentException => Failure(s"${s} must be either 'true' or 'false'")
      case NonFatal(e) => Failure(e.getMessage)
    }
}

object string extends StringInstances with StringFunctions {
  object stringSyntax extends scalaz.syntax.std.ToStringOps
}
