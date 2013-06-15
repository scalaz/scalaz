package scalaz
package concurrent

import scalaz.Spec
import collection.mutable
import java.util.concurrent._
import ConcurrentTest._

class ActorTest extends Spec {
  val NumOfMessages = 1000
  val NumOfThreads = 4
  val NumOfMessagesPerThread = NumOfMessages / NumOfThreads
  implicit val executor = Executors.newFixedThreadPool(NumOfThreads)

  "code executes async" in {
    val latch = new CountDownLatch(1)
    val actor = Actor[Int]((i: Int) => latch.countDown())
    actor ! 1
    assertCountDown(latch, "Should process a message")
  }

  "code errors are catched and can be handled" in {
    val latch = new CountDownLatch(1)
    val actor = Actor[Int]((i: Int) => 100 / i, (ex: Throwable) => latch.countDown())
    actor ! 0
    assertCountDown(latch, "Should catch an exception")
  }

  "actors exchange messages without loss" in {
    val latch = new CountDownLatch(NumOfMessages)
    var actor1: Actor[Int] = null
    val actor2 = Actor[Int]((i: Int) => actor1 ! i - 1)
    actor1 = Actor[Int] {
      (i: Int) =>
        if (i == latch.getCount) {
          if (i != 0) actor2 ! i - 1
          latch.countDown()
          latch.countDown()
        }
    }
    actor1 ! NumOfMessages
    assertCountDown(latch, "Should exchange " + NumOfMessages + " messages")
  }

  "actor handles messages in order of sending by each thread" in {
    val latch = new CountDownLatch(NumOfMessages)
    val actor = countingDownActor(latch)
    for (j <- 1 to NumOfThreads) fork {
      for (i <- 1 to NumOfMessagesPerThread) {
        actor ! (j, i)
      }
    }
    assertCountDown(latch, "Should process " + NumOfMessages + " messages")
  }

  def countingDownActor(latch: CountDownLatch): Actor[(Int, Int)] = Actor[(Int, Int)] {
    val ms = mutable.Map[Int, Int]()

    (m: (Int, Int)) =>
      val (j, i) = m
      if (ms.getOrElse(j, 0) + 1 == i) {
        ms.put(j, i)
        latch.countDown()
      }
  }
}
