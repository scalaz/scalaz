package scalaz
package xml
package cursor

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tag._

class TagTest extends Spec {
  checkAll(equal.laws[Tag])
  checkAll(lens.laws(nameTagL))
  checkAll(lens.laws(attribsTagL))
  checkAll(lens.laws(lineTagL))
}
