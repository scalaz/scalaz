package scalaz.test
package tests

import scala._, scala.Predef._

import scalaz.Void
import scalaz.data.IList
import scalaz.effect.{ IO, SafeApp }

object Main extends SafeApp {
  def run(args: List[String]): IO[Void, ExitStatus] = {
    val _ = args
    val suites: IList[() => Suite] = IList(
      () => new ExhaustiveSuite,
      () => new PureSuiteMetaSuite,
      () => new SuiteUtilsSuite
    )
    Runner(suites).map(succeeded => if (succeeded) ExitStatus.ExitNow(0) else ExitStatus.ExitNow(1))
  }
}
