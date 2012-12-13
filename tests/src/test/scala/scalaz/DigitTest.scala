package scalaz

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import scalaz.testlib.ScalaCheckBinding._
import org.scalacheck.{Gen, Arbitrary}

class DigitTest extends testlib.Spec {
  checkAll(order.laws[Digit])
}
