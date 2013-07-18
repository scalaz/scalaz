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
    "have a run method that returns" in {
      "when constructed from Future.now" in prop{(n: Int) =>
        Future.now(n).run must_== n
      }
      "when constructed from Future.delay" in prop{(n: Int) =>
        Future.delay(n).run must_== n
      }
      "when constructed from Future.fork" in prop{(n: Int) =>
        Future.fork(Future.now(n)).run must_== n
      }
      "when constructed from Future.suspend" in prop{(n: Int) =>
        Future.suspend(Future.now(n)).run must_== n
      }
      "when constructed from Future.async" in prop{(n: Int) =>
        def callback(call: Int => Unit): Unit = call(n)
        Future.async(callback).run must_== n
      }
      "when constructed from Future.apply" in prop{(n: Int) =>
        Future.apply(n).run must_== n
      }
    }
  }

  "Nondeterminism[Future]" should {
    import scalaz.concurrent.Future._
    implicit val es = Executors.newFixedThreadPool(1)
   
    "correctly process gatherUnordered for >1 futures in non-blocking way" in {
      val f1 = fork(now(1))(es)
      val f2 = delay(7).flatMap(_=>fork(now(2))(es))
      val f3 = fork(now(3))(es)
      
      val f = fork(Future.gatherUnordered(Seq(f1,f2,f3)))(es)
      
      f.run.toSet must_== Set(1,2,3)
    }


    "correctly process gatherUnordered for 1 future in non-blocking way" in {
      val f1 = fork(now(1))(es) 

      val f = fork(Future.gatherUnordered(Seq(f1)))(es)

      f.run.toSet must_== Set(1)
    }

    "correctly process gatherUnordered for empty seq of futures in non-blocking way" in {
      val f = fork(Future.gatherUnordered(Seq()))(es)

      f.run.toSet must_== Set()
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

