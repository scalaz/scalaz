package scalaz
package xml

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class AttrTest extends Spec {
  checkAll(order.laws[Attr])
  checkAll(lens.laws(Attr.keyAttrL))
  checkAll(lens.laws(Attr.valueAttrL))
}
