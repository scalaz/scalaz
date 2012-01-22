package scalaz
package typelevel.formatters

import typelevel.Formatter._

trait General {

  def bool(width: Int = 0, maxLength: Int = 0, left: Boolean = false) = new Fmt[Any] {
    def apply(x: Any) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (width > 0) width else "") +
        (if (maxLength > 0) "."+maxLength else "") +
        "b",
        x.asInstanceOf[java.lang.Object]
      )
    }
  }

  def hex(width: Int = 0, maxLength: Int = 0, left: Boolean = false) = new Fmt[Any] {
    def apply(x: Any) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (width > 0) width else "") +
        (if (maxLength > 0) "."+maxLength else "") +
        "h",
        x.asInstanceOf[java.lang.Object]
      )
    }
  }

  def str(width: Int = 0, maxLength: Int = 0, left: Boolean = false) = new Fmt[Any] {
    def apply(x: Any) = {
      javaFormatter.write(
        "%" +
        (if (left) "-" else "") +
        (if (width > 0) width else "") +
        (if (maxLength > 0) "."+maxLength else "") +
        "s",
        x.asInstanceOf[java.lang.Object]
      )
    }
  }

}

object General extends General
