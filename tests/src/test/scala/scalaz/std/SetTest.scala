package scalaz
package std

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import org.scalacheck.Prop.forAll

class SetTest extends testlib.Spec {
  checkAll(equal.laws[Set[Int]])
  checkAll(monadPlus.strongLaws[Option])
  checkAll(traverse.laws[Option])
}
