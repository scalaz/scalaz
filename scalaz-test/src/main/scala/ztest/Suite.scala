package scalaz.test

import java.lang.String
import scala.Boolean

import scalaz.core.EqClass
import scalaz.Void
import scalaz.Scalaz._
import scalaz.data.IList
import scalaz.effect.IO

trait Suite {
  def run: IO[Void, SuiteOutput]
}

final case class SuiteOutput(lines: IList[String], allSucceeded: Boolean)

object SuiteOutput {
  implicit val eqSuiteOutput: Eq[SuiteOutput] = instanceOf(new EqClass[SuiteOutput] {
    def equal(a: SuiteOutput, b: SuiteOutput): Boolean =
      a.allSucceeded == b.allSucceeded &&
        a.lines === b.lines
  })
}

object Suite {
  def reverse[A](as: IList[A]): IList[A] =
    as.foldLeft(IList.empty[A])((b, a) => IList.cons(a, b))
  def printScope(scope: IList[String]): String =
    util.fastConcatDelim(reverse(scope), "->")
  def printTest(scope: IList[String], out: TestResult) = out match {
    case _: Success.type => printScope(IList.cons("succeeded\n", scope))
    case _: Failure.type => printScope(IList.cons("failed\n", scope))
    case _: Thrown       => printScope(IList.cons("errored\n", scope))
  }
}
