package scalaz
package std

import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import std.AllInstances._

class IterableTest extends testlib.Spec {

  import std.iterable._

  checkAll(order.laws[Iterable[Boolean]].withProp("benchmark", order.scalaOrdering[Iterable[Boolean]]))
}