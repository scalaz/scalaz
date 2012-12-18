package scalaz
package xml

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import QName._

class QNameTest extends Spec {
  checkAll(order.laws[QName])
  checkAll(lens.laws(nameQNameL))
  checkAll(lens.laws(uriQNameL))
  checkAll(lens.laws(prefixQNameL))
}
