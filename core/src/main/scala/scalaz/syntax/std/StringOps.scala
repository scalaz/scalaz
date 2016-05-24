package scalaz
package syntax
package std

import scalaz.std.{string => s}

final class StringOps(val self: String) extends AnyVal {
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

  @deprecated("Unsafe. Use `charsNel` instead", "7.2")
  def charsNelErr(message: => String): NonEmptyList[Char] = s.charsNelErr(self, message)

  @deprecated("Unsafe. Use `charsNel` instead", "7.2")
  def unsafeCharsNel : NonEmptyList[Char] = s.unsafeCharsNel(self)

  // Parsing functions.

  def parseBoolean: Validation[IllegalArgumentException, Boolean] = s.parseBoolean(self)

  def parseByte: Validation[NumberFormatException, Byte] = s.parseByte(self)

  def parseShort: Validation[NumberFormatException, Short] = s.parseShort(self)

  def parseInt: Validation[NumberFormatException, Int] = s.parseInt(self)

  def parseLong: Validation[NumberFormatException, Long] = s.parseLong(self)

  def parseFloat: Validation[NumberFormatException, Float] = s.parseFloat(self)

  def parseDouble: Validation[NumberFormatException, Double] = s.parseDouble(self)

  def parseBigInt: Validation[NumberFormatException, BigInt] = s.parseBigInt(self)

  def parseBigDecimal: Validation[NumberFormatException, BigDecimal] = s.parseBigDecimal(self)
}

trait ToStringOps {
  implicit def ToStringOpsFromString(a:String): StringOps = new StringOps(a)
}
