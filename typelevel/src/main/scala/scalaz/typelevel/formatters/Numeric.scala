package scalaz
package typelevel.formatters

import java.lang.Integer
import java.math.BigInteger

import typelevel.Formatter._

trait Numeric {
  def decimalB(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Byte} =
    unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets).deunion

  def decimalS(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Short} =
    unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets).deunion

  def decimalI(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Integer} =
    unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets).deunion

  def decimalL(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               separators: Boolean = false,
               sign: Boolean = false,
               space: Boolean = false,
               brackets: Boolean = false): Format{type Source = Long} =
    unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets).deunion

  def decimalBI(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                separators: Boolean = false,
                sign: Boolean = false,
                space: Boolean = false,
                brackets: Boolean = false): Format{type Source = BigInteger} =
    unified.Numeric.decimal(width, left, padding, separators, sign, space, brackets).deunion


  def octalB(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Byte} =
    unified.Numeric.octal(width, left, padding, indicator).deunion

  def octalS(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Short} =
    unified.Numeric.octal(width, left, padding, indicator).deunion

  def octalI(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Integer} =
    unified.Numeric.octal(width, left, padding, indicator).deunion

  def octalL(width: Int = 0,
             left: Boolean = false,
             padding: Boolean = false,
             indicator: Boolean = false): Format{type Source = Long} =
    unified.Numeric.octal(width, left, padding, indicator).deunion

  def octalBI(width: Int = 0,
              left: Boolean = false,
              padding: Boolean = false,
              indicator: Boolean = false): Format{type Source = BigInteger} =
    unified.Numeric.octal(width, left, padding, indicator).deunion

  def hexDeciB(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Byte} =
    unified.Numeric.hexDeci(width, left, padding, indicator).deunion

  def hexDeciS(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Short} =
    unified.Numeric.hexDeci(width, left, padding, indicator).deunion

  def hexDeciI(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Integer} =
    unified.Numeric.hexDeci(width, left, padding, indicator).deunion

  def hexDeciL(width: Int = 0,
               left: Boolean = false,
               padding: Boolean = false,
               indicator: Boolean = false): Format{type Source = Long} =
    unified.Numeric.hexDeci(width, left, padding, indicator).deunion

  def hexDeciBI(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = BigInteger} =
    unified.Numeric.hexDeci(width, left, padding, indicator).deunion

  def uHexDeciB(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Byte} =
    unified.Numeric.uHexDeci(width, left, padding, indicator).deunion

  def uHexDeciS(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Short} =
    unified.Numeric.uHexDeci(width, left, padding, indicator).deunion

  def uHexDeciI(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Integer} =
    unified.Numeric.uHexDeci(width, left, padding, indicator).deunion

  def uHexDeciL(width: Int = 0,
                left: Boolean = false,
                padding: Boolean = false,
                indicator: Boolean = false): Format{type Source = Long} =
    unified.Numeric.uHexDeci(width, left, padding, indicator).deunion

  def uHexDeciBI(width: Int = 0,
                 left: Boolean = false,
                 padding: Boolean = false,
                 indicator: Boolean = false): Format{type Source = BigInteger} =
    unified.Numeric.uHexDeci(width, left, padding, indicator).deunion

  def cScientificF(width: Int = 0,
                   precision: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Float} =
    unified.Numeric.cScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def cScientificD(width: Int = 0,
                   precision: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Double} =
    unified.Numeric.cScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def cScientificBD(width: Int = 0,
                    precision: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = BigDecimal} =
    unified.Numeric.cScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def uCScientificF(width: Int = 0,
                    precision: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = Float} =
    unified.Numeric.uCScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def uCScientificD(width: Int = 0,
                    precision: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = Double} =
    unified.Numeric.uCScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def uCScientificBD(width: Int = 0,
                     precision: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     separators: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false,
                     brackets: Boolean = false): Format{type Source = BigDecimal} =
    unified.Numeric.uCScientific(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def scientificF(width: Int = 0,
                  magnitude: Int = 0,
                  left: Boolean = false,
                  decimalPoint: Boolean = false,
                  padding: Boolean = false,
                  separators: Boolean = false,
                  sign: Boolean = false,
                  space: Boolean = false,
                  brackets: Boolean = false): Format{type Source = Float} =
    unified.Numeric.scientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def scientificD(width: Int = 0,
                  magnitude: Int = 0,
                  left: Boolean = false,
                  decimalPoint: Boolean = false,
                  padding: Boolean = false,
                  separators: Boolean = false,
                  sign: Boolean = false,
                  space: Boolean = false,
                  brackets: Boolean = false): Format{type Source = Double} =
    unified.Numeric.scientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def scientificBD(width: Int = 0,
                   magnitude: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = BigDecimal} =
    unified.Numeric.scientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def uScientificF(width: Int = 0,
                   magnitude: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Float} =
    unified.Numeric.uScientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def uScientificD(width: Int = 0,
                   magnitude: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false): Format{type Source = Double} =
    unified.Numeric.uScientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def uScientificBD(width: Int = 0,
                    magnitude: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    separators: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false,
                    brackets: Boolean = false): Format{type Source = BigDecimal} =
    unified.Numeric.uScientific(width, magnitude, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def floatF(width: Int = 0,
             precision: Int = 0,
             left: Boolean = false,
             decimalPoint: Boolean = false,
             padding: Boolean = false,
             separators: Boolean = false,
             sign: Boolean = false,
             space: Boolean = false,
             brackets: Boolean = false): Format{type Source = Float} =
    unified.Numeric.float(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def floatD(width: Int = 0,
             precision: Int = 0,
             left: Boolean = false,
             decimalPoint: Boolean = false,
             padding: Boolean = false,
             separators: Boolean = false,
             sign: Boolean = false,
             space: Boolean = false,
             brackets: Boolean = false): Format{type Source = Double} =
    unified.Numeric.float(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def floatBD(width: Int = 0,
              precision: Int = 0,
              left: Boolean = false,
              decimalPoint: Boolean = false,
              padding: Boolean = false,
              separators: Boolean = false,
              sign: Boolean = false,
              space: Boolean = false,
              brackets: Boolean = false): Format{type Source = BigDecimal} =
    unified.Numeric.float(width, precision, left, decimalPoint, padding, separators, sign, space, brackets).deunion

  def floatHexDeciF(width: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false): Format{type Source = Float} =
    unified.Numeric.floatHexDeci(width, left, decimalPoint, padding, sign, space).deunion

  def floatHexDeciD(width: Int = 0,
                    left: Boolean = false,
                    decimalPoint: Boolean = false,
                    padding: Boolean = false,
                    sign: Boolean = false,
                    space: Boolean = false): Format{type Source = Double} =
    unified.Numeric.floatHexDeci(width, left, decimalPoint, padding, sign, space).deunion

  def floatHexDeciBD(width: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false): Format{type Source = BigDecimal} =
    unified.Numeric.floatHexDeci(width, left, decimalPoint, padding, sign, space).deunion

  def uFloatHexDeciF(width: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false): Format{type Source = Float} =
    unified.Numeric.uFloatHexDeci(width, left, decimalPoint, padding, sign, space).deunion

  def uFloatHexDeciD(width: Int = 0,
                     left: Boolean = false,
                     decimalPoint: Boolean = false,
                     padding: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false): Format{type Source = Double} =
    unified.Numeric.uFloatHexDeci(width, left, decimalPoint, padding, sign, space).deunion

  def uFloatHexDeciBD(width: Int = 0,
                      left: Boolean = false,
                      decimalPoint: Boolean = false,
                      padding: Boolean = false,
                      sign: Boolean = false,
                      space: Boolean = false): Format{type Source = BigDecimal} =
    unified.Numeric.uFloatHexDeci(width, left, decimalPoint, padding, sign, space).deunion
}

object Numeric extends Numeric
