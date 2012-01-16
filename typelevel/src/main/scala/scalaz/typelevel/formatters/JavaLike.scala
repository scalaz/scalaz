package scalaz
package typelevel.formatters

import typelevel.Formatter._

object JavaLike {
  type c = unified.String.char

  type d = unified.Numeric.decimal
  type o = unified.Numeric.octal
  type x = unified.Numeric.hexDeci
  type X = unified.Numeric.uHexDeci

  type e = unified.Numeric.cScientific
  type E = unified.Numeric.uCScientific
  type g = unified.Numeric.scientific
  type G = unified.Numeric.uScientific
  type f = unified.Numeric.float
  type a = unified.Numeric.floatHexDeci
  type A = unified.Numeric.uFloatHexDeci

  def b = General.bool _
  def h = General.hex _
  def s = General.str _
}
