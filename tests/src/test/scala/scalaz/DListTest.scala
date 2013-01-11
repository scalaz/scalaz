package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class DListTest extends Spec {
  checkAll(equal.laws[DList[Int]])
  checkAll(monoid.laws[DList[Int]])
  checkAll(monadPlus.laws[DList])
  check((0 to 100000).foldLeft(DList[Int]())(_ :+ _).toList == (0 to 100000).toList)
}
