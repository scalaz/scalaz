package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class ArrayTest extends Spec {

  checkAll(order.laws[Array[Int]])
  checkAll(plus.laws[Array])
  checkAll(monoid.laws[Array[Int]])

}

