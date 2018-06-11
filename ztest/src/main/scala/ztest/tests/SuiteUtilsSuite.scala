package ztest
package tests

import scala.Unit
import scala.Predef.String

import scalaz.Monad
import scalaz.data.IList
import scalaz.Scalaz._

final class SuiteUtilsSuite extends PureSuite {
  def doTests[G[_]: Monad](harness: Harness[() => ?, G]): G[Unit] = {
    def testResults =
      IList(
        () => (Failure(), "failed"),
        () => (Success(), "succeeded"),
        () => (Thrown(new java.lang.Exception()), "errored"))
    import harness._
    def withTestResult(pair: (TestResult, String)): G[Unit] =
      namedTest("Suite.printTest " + pair._2) { () =>
        val out = Suite.printTest(IList("bottom level", "mid level", "top level"), pair._1)

        if (out === "top level->mid level->bottom level->" + pair._2 + "\n") {
          Success()
        } else {
          Failure()
        }
      }

    section("suite utils") {
      property.exhaustive(testResults)(withTestResult) *>
      namedTest("Suite.printScope") { () =>
        val out = Suite.printScope(IList("bottom", "mid", "top"))
        if (out === "top->mid->bottom") Success()
        else Failure()
      }
    }
  }
}
