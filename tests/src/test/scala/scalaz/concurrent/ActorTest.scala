package scalaz
package concurrent

import collection.mutable
import java.util.concurrent._
import ConcurrentTest._

object ActorTest extends SpecLite {
  val NumOfMessages = 1000
  val NumOfThreads = 4
  val NumOfMessagesPerThread = NumOfMessages / NumOfThreads
  implicit val executor = Executors.newFixedThreadPool(NumOfThreads)

  "Actors" should {
    "handle messages async" in {
      val latch = new CountDownLatch(2)
      val actor = Actor[Int]((i: Int) => {
        latch.countDown()
        Thread.sleep(50)
        latch.countDown()
      })
      actor ! 1
      latch.await(1, TimeUnit.MILLISECONDS) must_== false
      latch.getCount must_== 1
      assertCountDown(latch, "Should handle a message")
    }

    "catch message handling errors" in {
      val latch = new CountDownLatch(1)
      val actor = Actor[Int]((i: Int) => throw new RuntimeException(), (ex: Throwable) => latch.countDown())
      actor ! 1
      assertCountDown(latch, "Should catch an exception")
    }

    "exchange messages without loss" in {
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

    "handle messages in order of sending by each thread" in {
      val latch = new CountDownLatch(NumOfMessages)
      val actor = countingDownActor(latch)
      for (j <- 1 to NumOfThreads) fork {
        for (i <- 1 to NumOfMessagesPerThread) {
          actor !(j, i)
        }
      }
      assertCountDown(latch, "Should handle " + NumOfMessages + " messages")
    }
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
