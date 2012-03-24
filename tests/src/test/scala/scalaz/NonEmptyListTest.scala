package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import Scalaz._

class NonEmptyListTest extends Specification with Sugar with ScalaCheck {

  "sumNel produces sum of NonEmptyList" verifies {
    (is: List[Int]) => {
      !(is.toNel map (_.sumNel === is.sum) exists (x => !x))
    }
  }

  "fold1Nel produces fold of NonEmptyList" verifies {
    (is: List[Int]) => {
      !(is.toNel map (_.foldl1Nel(_ + _) === is.sum) exists (x => !x))
    }
  }

}