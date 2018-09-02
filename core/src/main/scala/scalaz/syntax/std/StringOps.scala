package scalaz
package syntax
package std

import _root_.java.math.{ BigDecimal, BigInteger }

import scalaz.std.{string => s}

final class StringOps(private val self: String) extends AnyVal {
  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(n: Long): String = s.plural(self, n)

  /**
   * Constructs a non-empty list with the value if it is not empty, otherwise, throws an error.
   */
  def charsNel : Option[NonEmptyList[Char]] = s.charsNel(self)

  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def charsNel(e: => NonEmptyList[Char]) : NonEmptyList[Char] = s.charsNel(self, e)

  // Parsing functions.

  def parseBoolean: Validation[String, Boolean] = s.parseBoolean(self)

  def parseByte: Validation[String, Byte] = s.parseByte(self)

  def parseShort: Validation[String, Short] = s.parseShort(self)

  def parseInt: Validation[String, Int] = s.parseInt(self)

  def parseLong: Validation[String, Long] = s.parseLong(self)

  def parseFloat: Validation[String, Float] = s.parseFloat(self)

  def parseDouble: Validation[String, Double] = s.parseDouble(self)

  def parseBigInt: Validation[String, BigInteger] = s.parseBigInt(self)

  def parseBigDecimal: Validation[String, BigDecimal] = s.parseBigDecimal(self)
}

trait ToStringOps {
  implicit def ToStringOpsFromString(a:String): StringOps = new StringOps(a)
}
