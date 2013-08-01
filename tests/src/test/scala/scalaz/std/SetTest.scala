package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class SetTest extends Spec {
  checkAll(order.laws[Set[Int]])
  checkAll(monoid.laws[Set[Int]])
  checkAll(isEmpty.laws[Set])
}
