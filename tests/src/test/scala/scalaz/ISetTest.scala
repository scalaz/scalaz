package scalaz

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

object SetTest extends SpecLite {
  import org.scalacheck.Arbitrary
  import scalaz.scalacheck.ScalaCheckBinding._
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazArbitrary._
  import std.anyVal._

  import ISet._

  checkAll(order.laws[ISet[Int]])
  checkAll(monoid.laws[ISet[Double]])

  checkAll(foldable.laws[ISet])
}
