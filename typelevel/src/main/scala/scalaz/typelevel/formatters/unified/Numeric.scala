package scalaz
package typelevel.formatters.unified

import java.math.BigInteger

import UnionTypes._
import typelevel.Formatter._

trait Numeric {

  case class decimal(width: Int = 0,
                     left: Boolean = false,
                     padding: Boolean = false,
                     separators: Boolean = false,
                     sign: Boolean = false,
                     space: Boolean = false,
                     brackets: Boolean = false) extends UnionFormat {
    type D = t[Byte]#t[Short]#t[Int]#t[Long]#t[BigInteger]
    def apply(x: Union[D]) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (sign) "+" else "") +
        (if (space) " " else "") +
        (if (padding) "0" else "") +
        (if (separators) "," else "") +
        (if (brackets) "(" else "") +
        (if (width > 0) width else "") +
        "d",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class octal(width: Int = 0,
                   left: Boolean = false,
                   padding: Boolean = false,
                   indicator: Boolean = false) extends UnionFormat {
    type D = t[Byte]#t[Short]#t[Int]#t[Long]#t[BigInteger]
    def apply(x: Union[D]) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (indicator) "#" else "") +
        (if (padding) "0" else "") +
        (if (width > 0) width else "") +
        "o",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class hexDeci(width: Int = 0,
                     left: Boolean = false,
                     padding: Boolean = false,
                     indicator: Boolean = false) extends UnionFormat {
    type D = t[Byte]#t[Short]#t[Int]#t[Long]#t[BigInteger]
    def apply(x: Union[D]) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (indicator) "#" else "") +
        (if (padding) "0" else "") +
        (if (width > 0) width else "") +
        "x",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class uHexDeci(width: Int = 0,
                      left: Boolean = false,
                      padding: Boolean = false,
                      indicator: Boolean = false) extends UnionFormat {
    type D = t[Byte]#t[Short]#t[Int]#t[Long]#t[BigInteger]
    def apply(x: Union[D]) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (indicator) "#" else "") +
        (if (padding) "0" else "") +
        (if (width > 0) width else "") +
        "X",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class cScientific(width: Int = 0,
                         precision: Int = 0,
                         left: Boolean = false,
                         decimalPoint: Boolean = false,
                         padding: Boolean = false,
                         separators: Boolean = false,
                         sign: Boolean = false,
                         space: Boolean = false,
                         brackets: Boolean = false) extends UnionFormat {
    type D = t[Float]#t[Double]#t[BigDecimal]
    def apply(x: Union[D]) = {
      javaFormatter.write(
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
        "e",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class uCScientific(width: Int = 0,
                          precision: Int = 0,
                          left: Boolean = false,
                          decimalPoint: Boolean = false,
                          padding: Boolean = false,
                          separators: Boolean = false,
                          sign: Boolean = false,
                          space: Boolean = false,
                          brackets: Boolean = false) extends UnionFormat {
    type D = t[Float]#t[Double]#t[BigDecimal]
    def apply(x: Union[D]) = {
      javaFormatter.write(
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
        "E",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class scientific(width: Int = 0,
                        magnitude: Int = 0,
                        left: Boolean = false,
                        decimalPoint: Boolean = false,
                        padding: Boolean = false,
                        separators: Boolean = false,
                        sign: Boolean = false,
                        space: Boolean = false,
                        brackets: Boolean = false) extends UnionFormat {
    type D = t[Float]#t[Double]#t[BigDecimal]
    def apply(x: Union[D]) = {
      javaFormatter.write(
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
        "g",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class uScientific(width: Int = 0,
                         magnitude: Int = 0,
                         left: Boolean = false,
                         decimalPoint: Boolean = false,
                         padding: Boolean = false,
                         separators: Boolean = false,
                         sign: Boolean = false,
                         space: Boolean = false,
                         brackets: Boolean = false) extends UnionFormat {
    type D = t[Float]#t[Double]#t[BigDecimal]
    def apply(x: Union[D]) = {
      javaFormatter.write(
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
        "G",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class float(width: Int = 0,
                   precision: Int = 0,
                   left: Boolean = false,
                   decimalPoint: Boolean = false,
                   padding: Boolean = false,
                   separators: Boolean = false,
                   sign: Boolean = false,
                   space: Boolean = false,
                   brackets: Boolean = false) extends UnionFormat {
    type D = t[Float]#t[Double]#t[BigDecimal]
    def apply(x: Union[D]) = {
      javaFormatter.write(
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
        "f",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class floatHexDeci(width: Int = 0,
                          left: Boolean = false,
                          decimalPoint: Boolean = false,
                          padding: Boolean = false,
                          sign: Boolean = false,
                          space: Boolean = false) extends UnionFormat {
    type D = t[Float]#t[Double]#t[BigDecimal]
    def apply(x: Union[D]) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (decimalPoint) "#" else "") +
        (if (sign) "+" else "") +
        (if (space) " " else "") +
        (if (padding) "0" else "") +
        (if (width > 0) width else "") +
        "a",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

  case class uFloatHexDeci(width: Int = 0,
                          left: Boolean = false,
                          decimalPoint: Boolean = false,
                          padding: Boolean = false,
                          sign: Boolean = false,
                          space: Boolean = false) extends UnionFormat {
    type D = t[Float]#t[Double]#t[BigDecimal]
    def apply(x: Union[D]) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (decimalPoint) "#" else "") +
        (if (sign) "+" else "") +
        (if (space) " " else "") +
        (if (padding) "0" else "") +
        (if (width > 0) width else "") +
        "A",
        x.value.asInstanceOf[java.lang.Object]
      )
    }
  }

}

object Numeric extends Numeric
