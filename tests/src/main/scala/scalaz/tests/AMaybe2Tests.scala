package scalaz
package tests

import Predef.Int
import data._

import Scalaz._
import AMaybe2.{ empty, just }

import testz._
import z._

object AMaybe2Tests {
  def tests[T](harness: Harness[T], sequence: IList[T] => T): T = {
    import harness._
    sequence(IList(
      section("debug instance")(
        test("AEmpty") { () =>
          assert(empty[(?, ?), (?, ?), Int].debugs === "AEmpty2")
        },
        test("AJust") { () =>
          assert(just[(?, ?), (?, ?), Int, Int, Int]((0, 1), (2, 3)).debugs === "AJust2((0, 1), (2, 3))")
        },
      ),
      test("eq instance") { () =>
        val testData =
          IList(
            empty[(?, ?), (?, ?), Int],
            just[(?, ?), (?, ?), Int, Int, Int]((0, 1), (2, 3)),
            just[(?, ?), (?, ?), Int, Int, Int]((2, 3), (4, 5)),
          )
        testData.cross(testData).foldMap {
          case (a, b) =>
            assert((a === b) == (a eq b))
        }
      },
      test("empty is cached") { () =>
        assert(empty[(?, ?), (?, ?), Int] eq empty[(?, ?), (?, ?), Int])
      }
    ))
  }
}
