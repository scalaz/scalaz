package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.{Gen, Arbitrary}
import java.util.Random
import scalacheck.{ScalazArbitrary, ScalaCheckBinding}

class ScalazArbitraryTest extends Specification with ScalaCheck {
  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._
  import Arbitrary._

  // TODO Update expectation and re-instate.
//  "arbitraryTree" in {
//    val tree = arbitrary[Tree[Int]].apply(Gen.Params(size = 6, new Random(0L)))
//    tree.get.drawTree must_== """0
//|
//+- 3
//|
//+- 6
//|  |
//|  +- -2147483648
//|  |  |
//|  |  `- 1
//|  |
//|  `- -2147483648
//|     |
//|     `- 0
//|
//+- 1
//|
//+- -2147483648
//|  |
//|  +- -1
//|  |
//|  `- 3
//|     |
//|     `- 0
//|
//`- 5
//   |
//   +- -2147483648
//   |  |
//   |  `- -1
//   |
//   +- -2
//   |  |
//   |  `- -1
//   |
//   `- -1
//"""
//  }
}