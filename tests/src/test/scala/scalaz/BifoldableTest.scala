package scalaz

import scalaz.scalacheck.ScalazProperties._
import std.AllInstances._

object BifoldableTest extends SpecLite {

  checkAll("BifoldableLaws", bifoldable.laws[Either] )
}