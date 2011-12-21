package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Prop.forAll

class SetTest extends Spec {
  checkAll(equal.laws[Set[Int]].withProp("benchmark", forAll((a1: Set[Int], a2: Set[Int]) => Equal[Set[Int]].equal(a1, a2) == (a1 == a2))))
  checkAll(monadPlus.laws[Option])
  checkAll(traverse.laws[Option])
}
