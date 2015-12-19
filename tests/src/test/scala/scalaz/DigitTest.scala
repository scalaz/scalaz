package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object DigitTest extends SpecLite {
  checkAll(order.laws[Digit])
}
