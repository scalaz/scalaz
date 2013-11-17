package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object DListTest extends SpecLite {
  checkAll(equal.laws[DList[Int]])
  checkAll(monoid.laws[DList[Int]])
  checkAll(monadPlus.laws[DList])
  "DList append" ! ((0 to 100000).foldLeft(DList[Int]())(_ :+ _).toList must_== (0 to 100000).toList)
}
