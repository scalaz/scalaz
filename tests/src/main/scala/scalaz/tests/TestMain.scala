package scalaz.tests

import java.util.concurrent.Executors
import scala.{ Array, List, Unit }
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

import java.lang.String

import testz._
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

    val harness: Harness[PureHarness.Uses[Unit]] =
      PureHarness.toHarness(
        PureHarness.make(
          (ls, tr) => Runner.printStrs(Runner.printTest(ls, tr), scala.Console.print)
        )
      )

    def runPure(name: String, tests: PureHarness.Uses[Unit]): () => Unit =
      tests((), List(name))

    def combineUses(fst: PureHarness.Uses[Unit], snd: PureHarness.Uses[Unit]): PureHarness.Uses[Unit] =
      (r, ls) => {
        val f = fst(r, ls)
        val s = snd(r, ls)
        () =>
          {
            f()
            s()
          }
      }

    val suites: List[() => Future[() => Unit]] = List(
      Future(runPure("IList Tests", (new IListTests).tests(harness, combineUses)))(ec),
      Future(runPure("ACatenable1 Tests", ACatenable1Tests.tests(harness, combineUses)))(ec),
      Future(runPure("Debug Interpolator Tests", DebugInterpolatorTest.tests(harness)))(ec),
      Future(runPure("Scala Map Tests", SMapTests.tests(harness)))(ec),
      Future(runPure("Double Tests", (new DoubleTests).tests(harness)))(ec),
    ).map(r => () => r)

    Await.result(Runner(suites, ec), Duration.Inf)

    val _ = executor.shutdownNow()

  }
}
