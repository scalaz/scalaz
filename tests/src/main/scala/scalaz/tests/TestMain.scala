package scalaz
package tests

import java.util.concurrent.Executors
import scala.{ inline, Array, Char, Int, List, Unit }
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

import java.lang.String

import testz._
import runner.TestOutput
import extras.DocHarness

object TestMain {
  def main(args: Array[String]): Unit = {

    val isCI = try {
      scala.sys.env("CI") == "true"
    } catch {
      case _: java.util.NoSuchElementException => false
    }

    val executor = Executors.newFixedThreadPool(if (isCI) 1 else 2)
    val ec       = ExecutionContext.fromExecutor(executor)

    @inline def suites[T, U](harness: Harness[T], run: (String, T) => U)(
      ec: ExecutionContext
    ): List[Future[U]] =
      List(
        Future(run("ACatenable1 Tests", ACatenable1Tests.tests(harness)))(ec),
        Future(run("AFix Tests", AFixTests.tests(harness)))(ec),
        Future(run("AList1 Tests", AList1Tests.tests(harness)))(ec),
        Future(run("AMaybe Tests", AMaybeTests.tests(harness)))(ec),
        Future(run("AMaybe2 Tests", AMaybe2Tests.tests(harness)))(ec),
        Future(run("Const Tests", ConstTests.tests(harness)))(ec),
        Future(run("Debug Interpolator Tests", DebugInterpolatorTest.tests(harness)))(ec),
        Future(run("Double Tests", DoubleTests.tests(harness)))(ec),
        Future(run("IList Tests", IListTests.tests(harness)))(ec),
        Future(run("Maybe Tests", MaybeTests.tests(harness)))(ec),
        Future(run("IO Tests", IOTests.tests(harness)))(ec),
        Future(run("Ordering Tests", OrderingTests.tests(harness)))(ec),
        Future(run("Scala Map Tests", SMapTests.tests(harness)))(ec),
      )

    try {
      if (List(args: _*) == List("show")) {
        val harness =
          DocHarness.make

        @scala.annotation.tailrec
        def times(ch: Char, i: Int, acc: Array[Char]): String =
          if (i == 0) {
            acc(i) = ch
            new String(acc)
          } else {
            acc(i) = ch
            times(ch, i - 1, acc)
          }

        @inline def printSuite(name: String, desc: DocHarness.Uses[Unit]): List[String] = {
          val sb = new ListBuffer[String]
          harness.namedSection(name)(desc)("", sb)
          val outSb = new ListBuffer[String]
          outSb += "\n"
          outSb += times('=', name.length - 1, new Array[Char](name.length))
          outSb ++= sb.result().map("\n" + _)
          outSb.result()
        }

        val result =
          Await.result(
            Future.sequence(suites(harness, printSuite)(ec))(
              scala.collection.breakOut,
              ec
            ),
            Duration.Inf
          )

        result.foreach(_.foreach(scala.Console.print))

        scala.Console.println()
      } else {
        val harness =
          PureHarness.makeFromPrinter(
            (ls, tr) => runner.printStrs(runner.printTest(ls, tr), scala.Console.print)
          )

        @inline def runPure(name: String, tests: PureHarness.Uses[Unit]): TestOutput =
          tests((), List(name))

        val mySuites =
          suites(harness, runPure)(ec)
            .map(r => () => r)

        val result = Await.result(runner(mySuites, scala.Console.print, ec), Duration.Inf)

        if (result.failed) throw new java.lang.Exception() {
          override def fillInStackTrace(): java.lang.Throwable = this
        }
      }
    } finally {
      val _ = executor.shutdownNow()
    }
  }
}
