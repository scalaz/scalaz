package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class OrderingTest extends Spec {
  checkAll("Ordering", enum.laws[Ordering])
}
