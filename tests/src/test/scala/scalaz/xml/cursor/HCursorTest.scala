package scalaz
package xml
package cursor

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import HCursor._

class HCursorTest extends Spec {
  checkAll(equal.laws[HCursor])
  checkAll(lens.laws(historyHCursorL))
  checkAll(lens.laws(cursorHCursorL))
}
