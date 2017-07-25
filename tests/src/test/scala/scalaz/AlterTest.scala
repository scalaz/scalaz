package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.AllInstances._

object AlterTest extends SpecLite {

  checkAll(equal.laws[Alter[IList, Int]])

  checkAll("Alter[IList, Int]", monoid.laws[Alter[IList, Int]])
  checkAll("Alter[Maybe, Int]", monoid.laws[Alter[Maybe, Int]])

  object instances {
    def semigroup[A: Monoid] = Semigroup[Alter[IList, A]]
    def monoid[A: Monoid] = Monoid[Alter[Maybe, A]]
  }
}
