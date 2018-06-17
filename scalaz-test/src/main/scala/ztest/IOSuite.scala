package scalaz.test

import scala._, scala.Predef._

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import scalaz.data.IList.cons
import scalaz.Void
import scalaz.data._
import scalaz.Scalaz._

import scalaz.effect.{ IO, IORef }

abstract class IOSuite[E] extends Suite {
  def printError(e: E): TestResult
  def doTests[G[_]: Monad](test: Harness[IO[E, ?], G]): G[Unit]

  def run: IO[Void, SuiteOutput] = {
    type TestEff[A] = ReaderT[IO[E, ?], IList[String], A]
    val buf  = IO.sync[Void, AtomicReference[IList[String]]](new AtomicReference[IList[String]](IList.empty))
    val done = IO.sync[Void, AtomicBoolean](new AtomicBoolean(true))

    def add(buf: AtomicReference[IList[String]], str: String): Unit = {
      val _ = buf.updateAndGet(xs => cons(str, xs))
    }

    def makeHarness(buf: AtomicReference[IList[String]], done: AtomicBoolean): Harness[IO[E, ?], TestEff] =
      new Harness[IO[E, ?], TestEff] {
        def test(assertion: IO[E, TestResult]): TestEff[Unit] =
          ReaderT(
            (ls: IList[String]) =>
              for {
                eref <- IORef[E, TestResult](null)
                _ <- assertion.onError {
                      case -\/(t) => eref.write(Thrown(t)).map(_ => if (done.get) done.set(false))
                      case \/-(e) => eref.write(printError(e)).map(_ => if (done.get) done.set(false))
                    }.flatMap(eref.write).attempt
                result <- eref.read
                _ <- if (null eq result) {
                      scala.sys.error("Internal scalaz.test error: assertion wasn't written to reference.")
                    } else {
                      IO.sync[E, Unit](add(buf, Suite.printTest(ls, result)))
                    }
              } yield ()
          )

        def shared[A](task: IO[E, ?][A]): ReaderT[IO[E, ?], IList[String], A] =
          ReaderT(_ => task)

        def section[A](name: String)(test1: ReaderT[IO[E, ?], IList[String], A]): ReaderT[IO[E, ?], IList[String], A] =
          ReaderT((ls: IList[String]) => test1(cons(name, ls)))
      }

    for {
      b <- buf
      d <- done
      _ <- this.doTests(makeHarness(b, d)).apply(IList.empty).attempt.flatMap {
            case -\/(e)  => IO.sync[Void, Unit](add(b, Suite.printTest(IList.empty, printError(e))))
            case \/-(es) => es.pure[IO[Void, ?]]
          }
      lines           = b.get
      doneAndNonEmpty = d.get && !IList.isEmpty(lines)
      res             <- IO.sync[Void, SuiteOutput](SuiteOutput(lines, doneAndNonEmpty))
    } yield res
  }
}
