package scalaz
package xml

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Element._

class ElementTest extends Spec {
  checkAll(equal.laws[Element])
  checkAll(lens.laws(nameElementL))
  checkAll(lens.laws(attribsElementL))
  checkAll(lens.laws(contentElementL))
  checkAll(lens.laws(lineElementL))
}
