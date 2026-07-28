package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object ApplicativePlusTest extends SpecLite {

  implicit val eqCodensityListInt = new Equal[Codensity[List, Int]] {
    def equal(f1: Codensity[List, Int], f2: Codensity[List, Int]): Boolean =
      f1.improve == f2.improve
  }

  implicit val codensityListAlt = {
    import ApplicativePlus._
    // obtain via ApplicativePlus[List]
    Alt[Codensity[List, ?]]
  }

  checkAll(alt.laws[Codensity[List, ?]])
}
