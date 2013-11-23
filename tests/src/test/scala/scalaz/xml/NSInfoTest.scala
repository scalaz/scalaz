package scalaz
package xml

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import NSInfo._
import org.scalacheck.Prop.forAll

object NSInfoTest extends SpecLite {
  checkAll(equal.laws[NSInfo])
  checkAll(lens.laws(prefixesNSInfoL))
  checkAll(lens.laws(uriNSInfoL))
}
