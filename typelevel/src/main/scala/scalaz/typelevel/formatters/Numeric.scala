package scalaz
package typelevel.formatters

import java.lang.Integer
import java.math.BigInteger

import typelevel.Formatter._
import unified.UnionFormat._

object Numeric {
  def decimalB(width: Int = 0,
               maxLength: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Byte} =
    deunion(unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets))

  def decimalS(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Short} =
    deunion(unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets))

  def decimalI(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Integer} =
    deunion(unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets))

  def decimalL(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Long} =
    deunion(unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets))

  def decimalBI(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                separators: Boolean = false,
                sign: Boolean = false,
                space: Boolean = false,
                brackets: Boolean = false): Format{type Source = BigInteger} =
    deunion(unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets))


  def octalB(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Byte} =
    deunion(unified.Numeric.octal(width, left, padding, indicator))

  def octalS(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Short} =
    deunion(unified.Numeric.octal(width, left, padding, indicator))

  def octalI(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Integer} =
    deunion(unified.Numeric.octal(width, left, padding, indicator))

  def octalL(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Long} =
    deunion(unified.Numeric.octal(width, left, padding, indicator))

  def octalBI(width: Int = 0,
              left: Boolean = false,
              padding: Boolean = false,
              indicator: Boolean = false): Format{type Source = BigInteger} =
    deunion(unified.Numeric.octal(width, left, padding, indicator))

  def hexDeciB(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Byte} =
    deunion(unified.Numeric.hexDeci(width, left, padding, indicator))

  def hexDeciS(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Short} =
    deunion(unified.Numeric.hexDeci(width, left, padding, indicator))

  def hexDeciI(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Integer} =
    deunion(unified.Numeric.hexDeci(width, left, padding, indicator))

  def hexDeciL(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Long} =
    deunion(unified.Numeric.hexDeci(width, left, padding, indicator))

  def hexDeciBI(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = BigInteger} =
    deunion(unified.Numeric.hexDeci(width, left, padding, indicator))

  def uHexDeciB(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Byte} =
    deunion(unified.Numeric.uHexDeci(width, left, padding, indicator))

  def uHexDeciS(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Short} =
    deunion(unified.Numeric.uHexDeci(width, left, padding, indicator))

  def uHexDeciI(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Integer} =
    deunion(unified.Numeric.uHexDeci(width, left, padding, indicator))

  def uHexDeciL(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Long} =
    deunion(unified.Numeric.uHexDeci(width, left, padding, indicator))

  def uHexDeciBI(width: Int = 0,
                 left: Boolean = false,
                 padding: Boolean = false,
                 indicator: Boolean = false): Format{type Source = BigInteger} =
    deunion(unified.Numeric.uHexDeci(width, left, padding, indicator))

  def cScientificF(width: Int = 0,
                   precision: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Float} =
    deunion(unified.Numeric.cScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def cScientificD(width: Int = 0,
                   precision: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Double} =
    deunion(unified.Numeric.cScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def cScientificBD(width: Int = 0,
                    precision: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = BigDecimal} =
    deunion(unified.Numeric.cScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def uCScientificF(width: Int = 0,
                    precision: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = Float} =
    deunion(unified.Numeric.uCScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def uCScientificD(width: Int = 0,
                    precision: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = Double} =
    deunion(unified.Numeric.uCScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def uCScientificBD(width: Int = 0,
                     precision: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     separators: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false,
                     brackets: Boolean = false): Format{type Source = BigDecimal} =
    deunion(unified.Numeric.uCScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def scientificF(width: Int = 0,
                  magnitude: Int = 0,
                  left: Boolean = false,
                  decimalPoint: Boolean = false,
                  padding: Boolean = false,
                  separators: Boolean = false,
                  sign: Boolean = false,
                  space: Boolean = false,
                  brackets: Boolean = false): Format{type Source = Float} =
    deunion(unified.Numeric.scientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets))

  def scientificD(width: Int = 0,
                  magnitude: Int = 0,
                  left: Boolean = false,
                  decimalPoint: Boolean = false,
                  padding: Boolean = false,
                  separators: Boolean = false,
                  sign: Boolean = false,
                  space: Boolean = false,
                  brackets: Boolean = false): Format{type Source = Double} =
    deunion(unified.Numeric.scientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets))

  def scientificBD(width: Int = 0,
                   magnitude: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = BigDecimal} =
    deunion(unified.Numeric.scientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets))

  def uScientificF(width: Int = 0,
                   magnitude: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Float} =
    deunion(unified.Numeric.uScientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets))

  def uScientificD(width: Int = 0,
                   magnitude: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Double} =
    deunion(unified.Numeric.uScientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets))

  def uScientificBD(width: Int = 0,
                    magnitude: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = BigDecimal} =
    deunion(unified.Numeric.uScientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets))

  def floatF(width: Int = 0,
             precision: Int = 0,
             left: Boolean = false,
             decimalPoint: Boolean = false,
             padding: Boolean = false,
             separators: Boolean = false,
             sign: Boolean = false,
             space: Boolean = false,
             brackets: Boolean = false): Format{type Source = Float} =
    deunion(unified.Numeric.float(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def floatD(width: Int = 0,
             precision: Int = 0,
             left: Boolean = false,
             decimalPoint: Boolean = false,
             padding: Boolean = false,
             separators: Boolean = false,
             sign: Boolean = false,
             space: Boolean = false,
             brackets: Boolean = false): Format{type Source = Double} =
    deunion(unified.Numeric.float(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def floatBD(width: Int = 0,
              precision: Int = 0,
              left: Boolean = false,
              decimalPoint: Boolean = false,
              padding: Boolean = false,
              separators: Boolean = false,
              sign: Boolean = false,
              space: Boolean = false,
              brackets: Boolean = false): Format{type Source = BigDecimal} =
    deunion(unified.Numeric.float(width, precision, left, decimalPoint, padding, separators, sign, space, brackets))

  def floatHexDeciF(width: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false): Format{type Source = Float} =
    deunion(unified.Numeric.floatHexDeci(width, left, decimalPoint, padding, sign, space))

  def floatHexDeciD(width: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false): Format{type Source = Double} =
    deunion(unified.Numeric.floatHexDeci(width, left, decimalPoint, padding, sign, space))

  def floatHexDeciBD(width: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false): Format{type Source = BigDecimal} =
    deunion(unified.Numeric.floatHexDeci(width, left, decimalPoint, padding, sign, space))

  def uFloatHexDeciF(width: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false): Format{type Source = Float} =
    deunion(unified.Numeric.uFloatHexDeci(width, left, decimalPoint, padding, sign, space))

  def uFloatHexDeciD(width: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false): Format{type Source = Double} =
    deunion(unified.Numeric.uFloatHexDeci(width, left, decimalPoint, padding, sign, space))

  def uFloatHexDeciBD(width: Int = 0,
                      left: Boolean = false,
                      decimalPoint: Boolean = false,
                      padding: Boolean = false,
                      sign: Boolean = false,
                      space: Boolean = false): Format{type Source = BigDecimal} =
    deunion(unified.Numeric.uFloatHexDeci(width, left, decimalPoint, padding, sign, space))
}
