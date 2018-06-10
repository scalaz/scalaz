package ztest

import scala._, scala.Predef._

import java.util.concurrent.atomic.AtomicReference

import scalaz.data.IList.cons
import scalaz.Void
import scalaz.data._
import scalaz.Scalaz._

import scalaz.effect.{IO, IORef}

abstract class IOSuite[E] extends Suite {
  def printError(e: E): IList[TestError]
  def doTests[G[_]: Monad](test: Harness[IO[E, ?], G]): G[Unit]
  def run: IO[Void, IList[String]] = {
    type TestEff[A] = ReaderT[IO[E, ?], IList[String], A]
    val buf = IO.sync[Void, AtomicReference[IList[String]]](new AtomicReference[IList[String]](IList.empty))

    def add(buf: AtomicReference[IList[String]], str: String): Unit = {
      val _ = buf.updateAndGet(xs => cons(str, xs))
    }

    def makeHarness(buf: AtomicReference[IList[String]]): Harness[IO[E, ?], TestEff] =
      new Harness[IO[E, ?], TestEff] {
        def test
          (assertion: IO[E, ?][IList[TestError]]
          ): TestEff[Unit] = {
          ReaderT((ls: IList[String]) =>
            for {
              eref <- IORef[E, IList[TestError]](IList.empty[TestError])
              _ <- assertion.onError {
                case -\/(t) => eref.write(IList.cons(Thrown(t), IList.empty))
                case \/-(e) => eref.write(printError(e))
              }.flatMap(eref.write)
              errors <- eref.read
              _ <- IO.sync[E, Unit](add(buf, Suite.printTest(ls, errors)))
            } yield ()
          )
        }

        def shared[A](task: IO[E, ?][A]): ReaderT[IO[E, ?], IList[String], A] =
          ReaderT(_ => task)

        def section[A]
          (name: String)
          (test1: ReaderT[IO[E, ?], IList[String], A]
          ): ReaderT[IO[E, ?], IList[String], A] =
        ReaderT((ls: IList[String]) => test1(cons(name, ls)))
    }

    buf.flatMap { b =>
      this.doTests(makeHarness(b)).apply(IList.empty).attempt.flatMap {
        case -\/(e) => IO.sync[Void, Unit](add(b, Suite.printTest(IList.empty, printError(e))))
        case \/-(es) => es.pure[IO[Void, ?]]
      } *> IO.sync[Void, IList[String]](b.get)
    }
  }
}
