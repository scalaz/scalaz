package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.Arbitrary
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}

class TraverseTest extends Specification with Sugar with ScalaCheck {

  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._

  "list traverse" verifies {
    (as: List[Int], f: (Int => Int)) => {
      as.foldMapDefault(f) === as.foldMap(f)
    }
  }

  "list sequence example" in {
    List(List(1, 2), List(3, 4)).sequence must be_==(List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
  }
}