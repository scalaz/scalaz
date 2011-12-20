package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class DListTest extends Spec {
  checkAll(equal.laws[DList[Int]])
  checkAll(monoid.laws[DList[Int]])
  checkAll(monad.laws[DList])
}
