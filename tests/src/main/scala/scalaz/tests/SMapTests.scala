package scalaz
package tests

import scala.Int
import scala.Predef.Map

import data._
import laws._
import Scalaz._

import testz._
import z._

object SMapTests {
  def tests[T](harness: Harness[T]): T = {
    import harness._

    namedSection("laws")(
      namedSection("semicategory laws")(
        test("compose associativity") { () =>
          IList(
            (Map((2, 3)), Map((1, 2)), Map((0, 1))),
            (Map((3, 6), (1, 5)), Map((2, 3)), Map((0, 1), (1, 2))),
            (Map((0, 1)), Map.empty[Int, Int], Map.empty[Int, Int]),
            (Map.empty[Int, Int], Map((0, 1)), Map.empty[Int, Int]),
            (Map.empty[Int, Int], Map.empty[Int, Int], Map((0, 1))),
          ).foldMap {
            case (fst, snd, thd) =>
              SemicategoryLaws.composeAssoc(fst, snd, thd)(assertEqualMaps)
          }
        },
      ),
    ),
  }
}
