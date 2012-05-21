package scalaz
package typelevel.formatters.string

import typelevel.Format

trait Strings {

  def subs(start: Int) = Format((s: String) => s substring start)

  def show[T : Show] = Format(Show[T].shows)

}

object Strings extends Strings

// vim: expandtab:ts=2:sw=2
