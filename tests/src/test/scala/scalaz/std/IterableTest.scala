package scalaz
package std

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class IterableTest extends Spec {

  import std.iterable._

  checkAll(order.laws[Iterable[Boolean]].withProp("benchmark", order.scalaOrdering[Iterable[Boolean]]))
}