package scalaz
package tests

import Predef._
import data.{ Cord, IList }
import Scalaz._

import testz.Harness
import z._

object DebugInterpolatorTest {
  def tests[T](harness: Harness[T]): T = {
    import harness._
    namedSection("debug instance resolution")(
      test("int") { () =>
        assertEqual(z"before ${1} after", Cord("before 1 after"))
      },
      test("string") { () =>
        assertEqual(z"before ${"foo"} after", Cord("before foo after"))
      },
      test("ilist") { () =>
        assertEqual(z"before ${IList(1, 2, 3)} after", Cord("before IList(1,2,3) after"))
      }
    )
  }
}
