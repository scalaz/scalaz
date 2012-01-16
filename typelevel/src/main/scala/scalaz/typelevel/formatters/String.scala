package scalaz
package typelevel.formatters

import typelevel.Formatter._
import unified.UnionFormat._

object String {
  def charC(width: Int = 0, left: Boolean = false): Format{type Source = Char} =
    deunion(unified.String.char(width, left))

  def charB(width: Int = 0, left: Boolean = false): Format{type Source = Byte} =
    deunion(unified.String.char(width, left))

  def charS(width: Int = 0, left: Boolean = false): Format{type Source = Short} =
    deunion(unified.String.char(width, left))

  def subs(start: Int) = new Format {
    type Source = String
    def apply(s: String) = s substring start
  }
}
