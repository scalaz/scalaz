package scalaz
package syntax
package std

import scalaz.std.string

trait StringOps extends Ops[String]{
  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(n: Long): String = string.plural(self, n)

  def encode(implicit c: CharSet): Array[Byte] = string.encode(self)

  /**
   * Constructs a non-empty list with the value if it is not empty, otherwise, throws an error.
   */
  def charsNel : Option[NonEmptyList[Char]] = string.charsNel(self)

  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def charsNel(e: => NonEmptyList[Char]) : NonEmptyList[Char] = string.charsNel(self, e)

  def charsNelErr(message: => String): NonEmptyList[Char] = string.charsNelErr(self, message)

  def unsafeCharsNel : NonEmptyList[Char] = string.unsafeCharsNel(self)

  // Parsing functions.

  def parseBoolean: Validation[NumberFormatException, Boolean] = string.parseBoolean(self)

  def parseByte: Validation[NumberFormatException, Byte] = string.parseByte(self)

  def parseShort: Validation[NumberFormatException, Short] = string.parseShort(self)

  def parseInt: Validation[NumberFormatException, Int] = string.parseInt(self)

  def parseLong: Validation[NumberFormatException, Long] = string.parseLong(self)

  def parseFloat: Validation[NumberFormatException, Float] = string.parseFloat(self)

  def parseDouble: Validation[NumberFormatException, Double] = string.parseDouble(self)
}

trait ToStringOps {
  implicit def ToStringOpsFromString(a:String): StringOps = new StringOps{
    def self = a
  }
}