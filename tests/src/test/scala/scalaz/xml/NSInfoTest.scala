package scalaz
package xml

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import NSInfo._

class NSInfoTest extends Spec {
  checkAll(equal.laws[NSInfo])
  checkAll(lens.laws(prefixesNSInfoL))
  checkAll(lens.laws(uriNSInfoL))
}
