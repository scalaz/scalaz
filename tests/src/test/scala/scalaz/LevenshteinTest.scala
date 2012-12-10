package scalaz

import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary
import scalaz.testlib.ScalazArbitrary.{stateTArb => _, _}
import std.AllInstances._

class LevenshteinTest extends testlib.Spec {
  "string distance" in {
    MetricSpace[String].distance("kitten", "sitting") must be_===(3)
  }

  checkAll(metricSpace.laws[String])
}