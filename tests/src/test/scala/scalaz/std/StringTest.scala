package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class StringTest extends Spec {
  checkAll("String", monoid.laws[String])

  checkAll("String", order.laws[String].withProp("benchmark", order.scalaOrdering[String]))
}
