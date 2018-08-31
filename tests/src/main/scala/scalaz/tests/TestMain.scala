package scalaz.tests

import java.util.concurrent.Executors
import scala.{ inline, Array, Char, Int, List, Unit }
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

import java.lang.String

import testz._
import extras.DocHarness
import testz.runner.Runner

object TestMain {
  def main(args: Array[String]): Unit = {

    val isCI = try {
      scala.sys.env("CI") == "true"
    } catch {
      case _: java.util.NoSuchElementException => false
    }

    val executor = Executors.newFixedThreadPool(if (isCI) 1 else 2)
    val ec       = ExecutionContext.fromExecutor(executor)

    @inline def suites[T, U](harness: Harness[T], combineUses: (T, T) => T, cont: (String, T) => U)(
      ec: ExecutionContext
    ): List[Future[U]] =
      List(
        Future(cont("ACatenable1 Tests", ACatenable1Tests.tests(harness, combineUses)))(ec),
        Future(cont("AFix Tests", AFixTests.tests(harness)))(ec),
        Future(cont("AList1 Tests", AList1Tests.tests(harness, combineUses)))(ec),
        Future(cont("AMaybe Tests", AMaybeTests.tests(harness, combineUses)))(ec),
        Future(cont("Debug Interpolator Tests", DebugInterpolatorTest.tests(harness)))(ec),
        Future(cont("Double Tests", (new DoubleTests).tests(harness)))(ec),
        Future(cont("IList Tests", (new IListTests).tests(harness, combineUses)))(ec),
        Future(cont("Scala Map Tests", SMapTests.tests(harness)))(ec),
      )

    try {
      if (List(args: _*) == List("show")) {
        val harness =
          PureHarness.toHarness(
            new DocHarness
          )

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
          harness.section(name)(desc)("", sb)
          val outSb = new ListBuffer[String]
          outSb += "\n"
          outSb += times('=', name.length - 1, new Array[Char](name.length))
          outSb ++= sb.result().map("\n" + _)
          outSb.result()
        }

        @inline def combineUses(fst: DocHarness.Uses[Unit], snd: DocHarness.Uses[Unit]): DocHarness.Uses[Unit] =
          (s, sb) => { fst(s, sb); snd(s, sb); }

        val result =
          Await.result(
            Future.sequence(suites[DocHarness.Uses[Unit], List[String]](harness, combineUses, printSuite)(ec))(
              scala.collection.breakOut,
              ec
            ),
            Duration.Inf
          )

        result.foreach(_.foreach(scala.Console.print))

        scala.Console.println()
      } else {
        val harness: Harness[PureHarness.Uses[Unit]] =
          PureHarness.toHarness(
            PureHarness.make((ls, tr) => Runner.printStrs(Runner.printTest(ls, tr), scala.Console.print))
          )

        @inline def combineUses(fst: PureHarness.Uses[Unit], snd: PureHarness.Uses[Unit]): PureHarness.Uses[Unit] =
          (r, ls) => TestOutput.combine(fst(r, ls), snd(r, ls))

        @inline def runPure(name: String, tests: PureHarness.Uses[Unit]): TestOutput =
          tests((), List(name))

        val mySuites = suites[PureHarness.Uses[Unit], TestOutput](harness, combineUses, runPure)(ec).map(r => () => r)

        val result = Await.result(Runner(mySuites, ec), Duration.Inf)

        if (result.failed) throw new java.lang.Exception() {
          override def fillInStackTrace(): java.lang.Throwable = this
        }
      }
    } finally {
      val _ = executor.shutdownNow()
    }
  }
}
