package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object DListTest extends SpecLite {
  checkAll(equal.laws[DList[Int]])
  checkAll(monoid.laws[DList[Int]])
  checkAll(traverse.laws[DList])
  checkAll(isEmpty.laws[DList])
  checkAll(monadPlus.strongLaws[DList])
  "DList append" ! ((0 to 100000).foldLeft(DList[Int]())(_ :+ _).toList must_== (0 to 100000).toList)
}
