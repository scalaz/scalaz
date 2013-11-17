package scalaz
package xml
package cursor

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import HCursor._
import org.scalacheck.Prop.forAll

object HCursorTest extends SpecLite {
  checkAll(equal.laws[HCursor])
  checkAll(lens.laws(historyHCursorL))
  checkAll(lens.laws(cursorHCursorL))
}
