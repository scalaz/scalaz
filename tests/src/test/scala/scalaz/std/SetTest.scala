package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Prop.forAll

class SetTest extends Spec {
  checkAll(equal.laws[Set[Int]])
  checkAll(monadPlus.strongLaws[Option])
  checkAll(traverse.laws[Option])
  checkAll(isEmpty.laws[Set])
}
