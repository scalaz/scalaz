package scalaz
package syntax
package std

import scalaz.std.string

trait StringV extends SyntaxV[String]{
  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(n: Long): String = string.plural(self, n)

  import xml._

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A: Show](prefix: String, attributes: MetaData, scope: NamespaceBinding, a: Option[A]): Node = string.node(self, prefix, attributes, scope, a)

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A: Show](prefix: String, a: Option[A]): Node = string.node(prefix, a)

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A: Show](a: Option[A]): Node = string.node(self, a)
  
  def |:|[A: Show](a: Option[A]): Node = node(a)


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

//  def readFile[X](x: X, f: (X, Byte) => X): X = string.readFile[X](self, x, f)

//  def readLines[X, Y](x: X, f: (X, Char) => X, y: Y, g: (Y, X) => Y):Unit = string.readLines[X, Y](self, x, f, y, g)

  // Parsing functions.

  def parseBoolean: Validation[NumberFormatException, Boolean] = string.parseBoolean(self)

  def parseByte: Validation[NumberFormatException, Byte] = string.parseByte(self)

  def parseShort: Validation[NumberFormatException, Short] = string.parseShort(self)

  def parseInt: Validation[NumberFormatException, Int] = string.parseInt(self)

  def parseLong: Validation[NumberFormatException, Long] = string.parseLong(self)

  def parseFloat: Validation[NumberFormatException, Float] = string.parseFloat(self)

  def parseDouble: Validation[NumberFormatException, Double] = string.parseDouble(self)
}

trait ToStringV {
  implicit def ToStringVFromString(a:String): StringV = new StringV{
    def self = a
  }
}