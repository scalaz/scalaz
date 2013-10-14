package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Prop.forAll

object SetTest extends SpecLite {
  checkAll(order.laws[Set[Int]])
  checkAll(monoid.laws[Set[Int]])
  checkAll(isEmpty.laws[Set])
}
