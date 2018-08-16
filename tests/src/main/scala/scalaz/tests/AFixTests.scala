package scalaz
package tests

import scala.{ AnyRef, Int }

import data._

import testz._

object AFixTests {
  def tests[T](harness: Harness[T]): T = {
    import harness._
    section("optimizations")(
      test("unfix should be an identity") { () =>
        val empty = AFix.fix(AList.empty[AFix[AList, ?, ?], Int])
        assert(AFix.unfix(empty).asInstanceOf[AnyRef] eq empty.asInstanceOf[AnyRef])
      },
      test("fix should be an identity") { () =>
        val empty = AList.empty[AFix[AList, ?, ?], Int]
        assert(AFix.fix(empty).asInstanceOf[AnyRef] eq empty.asInstanceOf[AnyRef])
      },
    ),
  }
}
