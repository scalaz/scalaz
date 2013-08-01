package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class DigitTest extends Spec {
  checkAll(order.laws[Digit])
}
