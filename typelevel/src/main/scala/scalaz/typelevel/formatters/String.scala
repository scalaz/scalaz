package scalaz
package typelevel.formatters

import typelevel.Formatter._

trait String {
  def charC(width: Int = 0, left: Boolean = false): Format{type Source = Char} =
    unified.String.char(width, left).deunion

  def charB(width: Int = 0, left: Boolean = false): Format{type Source = Byte} =
    unified.String.char(width, left).deunion

  def charS(width: Int = 0, left: Boolean = false): Format{type Source = Short} =
    unified.String.char(width, left).deunion

  def subs(start: Int) = new Format {
    type Source = java.lang.String
    def apply(s: Source) = s substring start
  }
}

object String extends String
