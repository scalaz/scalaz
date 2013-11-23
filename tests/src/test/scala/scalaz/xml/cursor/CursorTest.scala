package scalaz
package xml
package cursor

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Cursor._
import org.scalacheck.Prop.forAll

object CursorTest extends SpecLite {
  checkAll(equal.laws[Cursor])
  checkAll(lens.laws(currentCursorL))
  checkAll(lens.laws(leftsCursorL))
  checkAll(lens.laws(rightsCursorL))
  checkAll(lens.laws(parentsCursorL))
}
