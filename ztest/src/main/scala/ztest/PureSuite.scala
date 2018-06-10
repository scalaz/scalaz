package ztest

import scala._, scala.Predef._

import java.util.concurrent.atomic.AtomicReference

import scalaz.Void
import scalaz.data.IList
import scalaz.Scalaz._
import scalaz.effect.IO

abstract class PureSuite extends Suite {
  def doTests[G[_]: Monad](test: Harness[() => ?, G]): G[Unit]

  type TestEff[A] = IList[String] => A

  def run: IO[Void, IList[String]] = IO.sync {
    val buf = new AtomicReference[IList[String]](IList.empty)
    val tester = this.doTests(PureSuite.makeHarness(buf))
    tester(IList.empty)
    buf.get
  }
}

object PureSuite {
  private def add(buf: AtomicReference[IList[String]], str: String): Unit = {
    val _ = buf.updateAndGet(IList.cons(str, _))
  }

  def makeHarness(buf: AtomicReference[IList[String]]): Harness[() => ?, IList[String] => ?] =
    new Harness[() => ?, IList[String] => ?] {
      def test(assertion: () => IList[TestError]): IList[String] => Unit =
        { (ls: IList[String]) =>
          add(buf, Suite.printTest(ls, assertion()))
        }

      def shared[A](fa: () => A): IList[String] => A =
        _ => fa()

      def section[A](name: String)(test: IList[String] => A): IList[String] => A =
        { (ls: IList[String]) =>
          test(IList.cons(name, ls))
        }
  }
}
