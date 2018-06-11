package ztest
package tests

import scala.Unit

import scalaz.{Monad, Void}
import scalaz.data.IList
import scalaz.Scalaz._

import scalaz.effect.IO

final class PureSuiteMetaSuite extends IOSuite[Void] {
  def printError(e: Void): TestResult = e.absurd
  def doTests[G[_]: Monad](harness: Harness[IO[Void, ?], G]): G[Unit] = {
    val suiteUnderTest = new PureSuite { self =>
      def doTests[H[_]: Monad](harness: Harness[() => ?, H]): H[Unit] = {
        import harness._
        section("top level") {
          namedTest("pure suite tests") { () =>
            Failure()
          } *>
          namedTest("pure suite tests 2") { () =>
            Success()
          }
        }
      }
    }
    import harness._

    section("PureSuite meta-suite") {
      test(for {
        output <- suiteUnderTest.run
        correctOutput = SuiteOutput(
          IList(Suite.printTest(IList("pure suite tests", "top level"), Failure()),
                Suite.printTest(IList("pure suite tests 2", "top level"), Success())),
          false)
      } yield
        if (correctOutput === output) Success()
        else Failure())
    }
  }
}
