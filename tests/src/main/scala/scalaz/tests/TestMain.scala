package scalaz.tests

import scala.{ Array, List, Unit }
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.global

import java.lang.String

import testz._
import testz.runner.Runner

object TestMain {
  def main(args: Array[String]): Unit = {
    val harness: Harness[PureHarness.Uses[Unit]] =
      PureHarness.toHarness(
        PureHarness.make(
          (ls, tr) => Runner.printStrs(Runner.printTest(ls, tr), scala.Console.print)
        )
      )

    def runPure(name: String, tests: PureHarness.Uses[Unit]): Future[() => Unit] =
      Future.successful(tests((), List(name)))

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
      () => runPure("IList Tests", (new IListTests).tests(harness, combineUses))
    )

    Await.result(Runner(suites, global), Duration.Inf)

  }
}
