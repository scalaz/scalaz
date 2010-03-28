package scalaz.example

import scalaz._

object ExampleBifunctor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val x: Either[Int, String] = Left(7)
    val y = (8, "abc")
    val z: Either[Int, String] = Right("def")

    val fr = (_: String).reverse
    val fl = (_: Int) + 1

    // Map reverse across the right of the Either binary functor
    (x :-> fr) assert_≟ Left(7)

    // Map (+1) across the left of the Either binary functor
    (fl <-: x) assert_≟ Left(8)

    // Map reverse across the right and (+1) across the left of the Either binary functor
    (fl <-: x :-> fr) assert_≟ Left(8)

    // Map reverse across the right of the Tuple2 (pair) binary functor
    (y :-> fr) assert_≟ (8, "cba")

    // Map (+1) across the left of the Tuple2 (pair) binary functor
    (fl <-: y) assert_≟ (9, "abc")

    // Map reverse across the right and (+1) across the left of the Tuple2 (pair) binary functor
    (fl <-: y :-> fr) assert_≟ (9, "cba")

    // Map reverse across the right of the Either binary functor
    (z :-> fr) assert_≟ Right("fed")

    // Map (+1) across the left of the Either binary functor
    (fl <-: z) assert_≟ Right("def")

    // Map reverse across the right and (+1) across the left of the Either binary functor
    (fl <-: z :-> fr) assert_≟ Right("fed")
  }
}