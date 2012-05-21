package scalaz
package typelevel.formatters.string

import UnionTypes._
import typelevel.UnionFormat

trait Numeric {

  type Integrals = t[Byte]#t[Short]#t[Int]#t[Long]#t[java.math.BigInteger]#t[scala.math.BigInt]
  type Floatings = t[Float]#t[Double]#t[java.math.BigDecimal]#t[scala.math.BigDecimal]

  class integral(fmt: String) extends UnionFormat[Integrals, String] {
    def apply(u: Union[Integrals]) = u.value match {
      case bi: scala.math.BigInt => fmt format bi.bigInteger
      case v => fmt format v
    }
  }

  class floating(fmt: String) extends UnionFormat[Floatings, String] {
    def apply(u: Union[Floatings]) = u.value match {
      case bd: scala.math.BigDecimal => fmt format bd.bigDecimal
      case v => fmt format v
    }
  }

  case class decimal(
    width: Int = 0,
    left: Boolean = false,
    padding: Boolean = false,
    separators: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false,
    brackets: Boolean = false
  ) extends integral(
    "%" +
    (if (left) "-" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (separators) "," else "") +
    (if (brackets) "(" else "") +
    (if (width > 0) width else "") +
    "d"
  )

  case class octal(
    width: Int = 0,
    left: Boolean = false,
    padding: Boolean = false,
    indicator: Boolean = false
  ) extends integral(
    "%" +
    (if (left) "-" else "") +
    (if (indicator) "#" else "") +
    (if (padding) "0" else "") +
    (if (width > 0) width else "") +
    "o"
  )

  case class hexDeci(
    width: Int = 0,
    left: Boolean = false,
    padding: Boolean = false,
    indicator: Boolean = false
  ) extends integral(
    "%" +
    (if (left) "-" else "") +
    (if (indicator) "#" else "") +
    (if (padding) "0" else "") +
    (if (width > 0) width else "") +
    "x"
  )

  case class uHexDeci(
    width: Int = 0,
    left: Boolean = false,
    padding: Boolean = false,
    indicator: Boolean = false
  ) extends integral(
    "%" +
    (if (left) "-" else "") +
    (if (indicator) "#" else "") +
    (if (padding) "0" else "") +
    (if (width > 0) width else "") +
    "X"
  )

  case class cScientific(
    width: Int = 0,
    precision: Int = 0,
    left: Boolean = false,
    decimalPoint: Boolean = false,
    padding: Boolean = false,
    separators: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false,
    brackets: Boolean = false
  ) extends floating(
    "%" +
    (if (left) "-" else "") +
    (if (decimalPoint) "#" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (separators) "," else "") +
    (if (brackets) "(" else "") +
    (if (width > 0) width else "") +
    (if (precision > 0) "."+precision else "") +
    "e"
  )

  case class uCScientific(
    width: Int = 0,
    precision: Int = 0,
    left: Boolean = false,
    decimalPoint: Boolean = false,
    padding: Boolean = false,
    separators: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false,
    brackets: Boolean = false
  ) extends floating(
    "%" +
    (if (left) "-" else "") +
    (if (decimalPoint) "#" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (separators) "," else "") +
    (if (brackets) "(" else "") +
    (if (width > 0) width else "") +
    (if (precision > 0) "."+precision else "") +
    "E"
  )

  case class scientific(
    width: Int = 0,
    magnitude: Int = 0,
    left: Boolean = false,
    decimalPoint: Boolean = false,
    padding: Boolean = false,
    separators: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false,
    brackets: Boolean = false
  ) extends floating(
    "%" +
    (if (left) "-" else "") +
    (if (decimalPoint) "#" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (separators) "," else "") +
    (if (brackets) "(" else "") +
    (if (width > 0) width else "") +
    (if (magnitude > 0) "."+magnitude else "") +
    "g"
  )

  case class uScientific(
    width: Int = 0,
    magnitude: Int = 0,
    left: Boolean = false,
    decimalPoint: Boolean = false,
    padding: Boolean = false,
    separators: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false,
    brackets: Boolean = false
  ) extends floating(
    "%" +
    (if (left) "-" else "") +
    (if (decimalPoint) "#" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (separators) "," else "") +
    (if (brackets) "(" else "") +
    (if (width > 0) width else "") +
    (if (magnitude > 0) "."+magnitude else "") +
    "G"
  )

  case class float(
    width: Int = 0,
    precision: Int = 0,
    left: Boolean = false,
    decimalPoint: Boolean = false,
    padding: Boolean = false,
    separators: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false,
    brackets: Boolean = false
  ) extends floating(
    "%" +
    (if (left) "-" else "") +
    (if (decimalPoint) "#" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (separators) "," else "") +
    (if (brackets) "(" else "") +
    (if (width > 0) width else "") +
    (if (precision > 0) "."+precision else "") +
    "f"
  )

  case class floatHexDeci(
    width: Int = 0,
    left: Boolean = false,
    decimalPoint: Boolean = false,
    padding: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false
  ) extends floating(
    "%" +
    (if (left) "-" else "") +
    (if (decimalPoint) "#" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (width > 0) width else "") +
    "a"
  )

  case class uFloatHexDeci(
    width: Int = 0,
    left: Boolean = false,
    decimalPoint: Boolean = false,
    padding: Boolean = false,
    sign: Boolean = false,
    space: Boolean = false
  ) extends floating(
    "%" +
    (if (left) "-" else "") +
    (if (decimalPoint) "#" else "") +
    (if (sign) "+" else "") +
    (if (space) " " else "") +
    (if (padding) "0" else "") +
    (if (width > 0) width else "") +
    "A"
  )

}

object Numeric extends Numeric

// vim: expandtab:ts=2:sw=2
