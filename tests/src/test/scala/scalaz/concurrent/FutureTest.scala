package scalaz
package concurrent

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.specs2.execute.{Failure, Result}
import java.util.concurrent._
import ConcurrentTest._

class FutureTest extends Spec {
  implicit def FutureEqual[A: Equal] =
    Equal[A].contramap((_: Future[A]).run)

  checkAll(monad.laws[Future])

  val non = Nondeterminism[Future]

  "Future" should {
    "not deadlock when using Nondeterminism#chooseAny" in {
      withTimeout(2000) {
        deadlocks(3).run.length must_== 4
      }
    }
  }

  /*
   * This is a little deadlock factory based on the code in #308.
   *
   * Basically it builds a tree of futures that run in a
   * non-determistic order. The sleep(x) provides an increase
   * in the number of collisions, and chance for deadlock.
   *
   * Before #312 patch, this triggered deadlock approx 1 in every
   * 3 runs.
   */
  def deadlocks(depth: Int): Future[List[Long]] =
    if (depth == 1)
      Future.fork(
        Future.delay({
          Thread.sleep(20)
          List(System.currentTimeMillis)
        })
      )
    else
      Future.fork(
        non.both(deadlocks(depth - 1), deadlocks(depth - 1)) map ({
          case (l, r) => l ++ r
        })
      )
}
