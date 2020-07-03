package scalaz
package std

import _root_.java.math.{ BigDecimal, BigInteger }

import scala.reflect.ClassTag
import scala.util.control.NonFatal

trait StringInstances {
  implicit val stringInstance: Monoid[String] with Show[String] with Order[String] with IsEmpty[λ[α => String]] =
    new Monoid[String] with Show[String] with Order[String] with IsEmpty[λ[α => String]] {
      type SA[α] = String
      def append(f1: String, f2: => String) = f1 + f2
      def zero: String = ""
      private[this] val cordDoubleQuote = Cord("\"")
      override def show(f: String): Cord = cordDoubleQuote :: Cord(f) :: cordDoubleQuote
      override def shows(f: String): String = s""""${f}""""
      def order(x: String, y: String) = Ordering.fromInt(x.compareTo(y))
      override def equal(x: String, y: String) = x == y
      override def equalIsNatural: Boolean = true
      def empty[A] = zero
      def plus[A](f1: SA[A], f2: => SA[A]) = f1 + f2
      def isEmpty[A](s: SA[A]) = s == ""
    }
}

trait StringFunctions {
  import string.stringInstance

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
  def charsNel(s:String) : Maybe[NonEmptyList[Char]] = list.toNel(s.toList)

  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def charsNel(s:String, e: => NonEmptyList[Char]) : NonEmptyList[Char] = charsNel(s) getOrElse e

  // Parsing functions.
  private def intChars(s: String): Boolean = s.length == 1 && s.charAt(0).isDigit || ((s.length > 1) && s.stripPrefix("-").forall(_.isDigit))

  // everything boxes... :-(
  def parseByte(s: String): Validation[String, Byte] = SafeNumbers.byte(s) match {
    case ByteNone => Failure(s"${s} does not represent a valid Byte")
    case ByteSome(i) => Success(i)
  }
  def parseShort(s: String): Validation[String, Short] = SafeNumbers.short(s) match {
    case ShortNone => Failure(s"${s} does not represent a valid Short")
    case ShortSome(i) => Success(i)
  }
  def parseInt(s: String): Validation[String, Int] = SafeNumbers.int(s) match {
    case IntNone => Failure(s"${s} does not represent a valid Int")
    case IntSome(i) => Success(i)
  }
  def parseLong(s: String): Validation[String, Long] = SafeNumbers.long(s) match {
    case LongNone => Failure(s"${s} does not represent a valid Long")
    case LongSome(i) => Success(i)
  }
  def parseBigInt(s: String): Validation[String, BigInteger] = SafeNumbers.biginteger(s) match {
    case None => Failure(s"${s} does not represent a valid BigInteger")
    case Some(i) => Success(i)
  }
  def parseFloat(s: String): Validation[String, Float] = SafeNumbers.float(s) match {
    case FloatNone => Failure(s"${s} does not represent a valid Float")
    case FloatSome(i) => Success(i)
  }
  def parseDouble(s: String): Validation[String, Double] = SafeNumbers.double(s) match {
    case DoubleNone => Failure(s"${s} does not represent a valid Double")
    case DoubleSome(i) => Success(i)
  }
  def parseBigDecimal(s: String): Validation[String, BigDecimal] = SafeNumbers.bigdecimal(s) match {
    case None => Failure(s"${s} does not represent a valid BigDecimal")
    case Some(i) => Success(i)
  }

  def parseBoolean(s: String): Validation[String, Boolean] =
    if (s.equalsIgnoreCase("true")) Success(true)
    else if (s.equalsIgnoreCase("false")) Success(false)
    else Failure(s"${s} must be either 'true' or 'false'")
}

object string extends StringInstances with StringFunctions {
  object stringSyntax extends scalaz.syntax.std.ToStringOps
}
