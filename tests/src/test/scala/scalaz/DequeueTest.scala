package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck._
import Arbitrary._
import Scalaz._

class DequeueTest extends Specification with Sugar with ScalaCheck {
  "appending one element works correctly" verifies {
    (xs: Dequeue[Int], x: Int) =>
      (xs ::> x).toList ≟ (xs.toList ::: x :: Nil)
  }

  "prepending one element works correctly" verifies {
    (xs: Dequeue[Int], x: Int) =>
      (x :: xs).toList ≟ (x :: xs.toList)
  }

  "combining two lists works correctly" verifies {
    (xs: Dequeue[Int], ys: Dequeue[Int]) =>
      (xs ::: ys).toList ≟ (xs.toList ::: ys.toList)
  }

  "multiple operations" verifies {
    (xs: Dequeue[Int], x: Int) => {
      val y = x * 2
      ((x :: xs) ::> y).toList ≟ ((x :: xs.toList) ::: y :: Nil)
    }
  }

  "append and prepend associative" verifies {
    (xs: Dequeue[Int], x: Int) => {
      val y = x * 2
      (x :: (xs ::> y)).toList ≟ ((x :: xs) ::> y).toList
    }
  }

  "List to Dequeue and back." verifies {
    (xs: List[Int]) => {
      xs ≟ xs.foldRight(Dequeue.empty[Int])((x, xs) => x :: xs).toList
    }
  }

  "append and prepend are consistent with List" verifies {
    (xs: Dequeue[Int], x: Int) => {
      val y = x * 2
      val l = (x :: (xs.toList ::: List(y)))
      val d = (x :: (xs ::> y))
      (d ::: d).toList ≟ (l ::: l)
    }
  }


}
