package scalaz
package tests

import Predef.Int
import data._

import Scalaz._
import AMaybe.{ empty, just }

import testz._
import z._

object AMaybeTests {
  def tests[T](harness: Harness[T]): T = {
    import harness._
    section(
      namedSection("instances")(
        namedSection("debug")(
          test("AEmpty") { () =>
            assert(empty[(?, ?), Int].debugs === "AEmpty")
          },
          test("AJust") { () =>
            assert(just[(?, ?), Int, Int]((0, 1)).debugs === "AJust((0, 1))")
          },
        ),
        test("eq") { () =>
          val testData =
            IList(
              empty[(?, ?), Int],
              just[(?, ?), Int, Int]((0, 1)),
              just[(?, ?), Int, Int]((2, 3)),
            )
          testData.cross(testData).foldMap {
            case (a, b) =>
              assert((a === b) == (a eq b))
          }
        },
      ),
      test("empty is cached") { () =>
        assert(empty[(?, ?), Int] eq empty[(?, ?), Int])
      },
    )
  }
}
