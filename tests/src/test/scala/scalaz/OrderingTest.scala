package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object OrderingTest extends SpecLite {
  checkAll("Ordering", enum.laws[Ordering])
  checkAll("Ordering", monoid.laws[Ordering])
}
