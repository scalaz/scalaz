package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import Scalaz._

class NonEmptyListTest extends Specification with Sugar with ScalaCheck {

  "non empty list" should {
    "support min" in {
      "with one element" in {
        nel(4, Nil).min must_== 4
      }
      "with many elements" in {
        nel(4, 2, 30, -11).min must_== -11
      }
    }
    "support max" in {
      "with one element" in {
        nel(4, Nil).max must_== 4
      }
      "with many elements" in {
        nel(4, 2, 30, -11).max must_== 30
      }
    }
    "support minBy" in {
      "with one element" in {
        nel("foo", Nil).minBy(_.size) must_== "foo"
      }
      "with many elements" in {
        nel("foo", "a", "scalaz", "scala").minBy(_.size) must_== "a"
      }
    }
    "support maxBy" in {
      "with one element" in {
        nel("foo", Nil).maxBy(_.size) must_== "foo"
      }
      "with many elements" in {
        nel("foo", "a", "scalaz", "scala").maxBy(_.size) must_== "scalaz"
      }
    }
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

}
