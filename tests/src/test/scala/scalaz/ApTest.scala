package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.AllInstances._

object ApTest extends SpecLite {

  checkAll(equal.laws[Ap[IList, Int]])

  checkAll("Ap[IList, Int]", monoid.laws[Ap[IList, Int]])
  checkAll("Ap[Maybe, Int]", monoid.laws[Ap[Maybe, Int]])

  object instances {
    def semigroup[A: Monoid] = Semigroup[Ap[IList, A]]
    def monoid[A: Monoid] = Monoid[Ap[Maybe, A]]
  }
}
