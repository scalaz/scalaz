package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.{Gen, Arbitrary}

class DigitTest extends Spec {
  checkAll(order.laws[Digit])
}
