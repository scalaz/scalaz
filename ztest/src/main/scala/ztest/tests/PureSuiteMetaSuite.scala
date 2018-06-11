package ztest
package tests

import scala.Unit

import scalaz.{Monad, Void}
import scalaz.data.IList
import scalaz.Scalaz._

import scalaz.effect.IO

final class PureSuiteMetaSuite extends IOSuite[Void] {
  def printError(e: Void): IList[TestError] = e.absurd
  def doTests[G[_]: Monad](harness: Harness[IO[Void, ?], G]): G[Unit] = {
    val suiteUnderTest = new PureSuite { self =>
      def doTests[H[_]: Monad](harness: Harness[() => ?, H]): H[Unit] = {
        import harness._
        namedTest("pure suite tests") { () =>
          IList(Failure("hey"), Failure("there"))
        }
      }
    }
    import harness._
    section("PureSuite meta-suite") {
      test(for {
        output <- suiteUnderTest.run
        correctOutput =
          IList(Suite.printTest(IList("pure suite tests"), IList(Failure("hey"), Failure("there"))))
      } yield if (correctOutput === output) IList.empty else IList(Failure("errors were incorrect")))

    }
  }
}
