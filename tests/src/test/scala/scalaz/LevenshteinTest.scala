package scalaz

import scalaz.scalacheck.ScalazProperties._
import std.AllInstances._

class LevenshteinTest extends Spec {
  "string distance" in {
    MetricSpace[String].distance("kitten", "sitting") must be_===(3)
  }

  checkAll(metricSpace.laws[String])
}
