package scalaz
package typelevel.formatters

import typelevel.Formatter._

trait String {

  def subs(start: Int) = new Fmt[java.lang.String]{
    def apply(s: java.lang.String) = s substring start
  }

}

object String extends String
