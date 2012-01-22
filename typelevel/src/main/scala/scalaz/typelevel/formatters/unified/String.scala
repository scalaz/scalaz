package scalaz
package typelevel.formatters.unified

import UnionTypes._
import typelevel.Formatter._

trait String {

  case class char(width: Int = 0, left: Boolean = false) extends UnionFormat[t[Char]#t[Byte]#t[Short]] {
    def apply(x: Union[D]) = {
      (
        "%" +
        (if (left) "-" else "") +
        (if (width > 0) width else "") +
        "c"
      ) format x.value.asInstanceOf[java.lang.Object]
    }
  }

}

object String extends String
