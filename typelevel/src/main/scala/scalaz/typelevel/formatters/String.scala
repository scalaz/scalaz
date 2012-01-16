package scalaz
package typelevel.formatters

import typelevel.Formatter._

trait String {

  def subs(start: Int) = new Format {
    type Source = java.lang.String
    def apply(s: Source) = s substring start
  }

}

object String extends String
