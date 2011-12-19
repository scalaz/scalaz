package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class ListTest extends Spec {
  checkAll("List", equal.laws[List[Int]])
  checkAll("List", monoid.laws[List[Int]])
  checkAll("List", monadPlus.laws[Option])
  checkAll("List", traverse.laws[Option])
}
