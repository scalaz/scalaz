package scalaz
package tests

import data._
import Scalaz._

import laws._

import testz._
import z._

object OrderingTests {
  def tests[T](harness: Harness[T]): T = {
    import harness._

    val allValues = IList(
      EQ,
      LT,
      GT
    )

    namedSection("instances")(
      namedSection("eq")(
        test("identity") { () =>
          allValues.cross(allValues).foldMap {
            case (o1, o2) =>
              assert((o1 == o2) == (o1 === o2))
          }
        },
        test("reflexivity") { () =>
          allValues.foldMap {
            EqLaws.reflexivity(_)(assert)
          }
        },
      ),
      test("debug") { () =>
        val table = IList(
          (EQ, "EQ"),
          (LT, "LT"),
          (GT, "GT"),
        )
        table.foldMap {
          case (i, o) => assert(i.debugs == o)
        }
      },
    ),

  }

}
