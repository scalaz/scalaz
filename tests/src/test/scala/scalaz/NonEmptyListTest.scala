package scalaz

import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import std.AllInstances._

class NonEmptyListTest extends testlib.Spec {
  checkAll("NonEmptyList", monad.laws[NonEmptyList])
  checkAll("NonEmptyList", plus.laws[NonEmptyList])
  checkAll("NonEmptyList", semigroup.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", equal.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", traverse.laws[NonEmptyList])
  checkAll("NonEmptyList", comonad.laws[NonEmptyList])

  "NonEmptyList size is corect" ! prop { xs:NonEmptyList[Int] =>
    xs.size must be_===(1 + xs.tail.size) 
  }
}
