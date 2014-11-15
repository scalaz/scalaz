package scalaz
package concurrent

import collection.mutable
import java.util.concurrent._
import ConcurrentTest._

object ActorTest extends SpecLite {
  val NumOfMessages = 1000
  val NumOfThreads = 4
  val NumOfMessagesPerThread = NumOfMessages / NumOfThreads
  val executor = Executors.newFixedThreadPool(NumOfThreads)
  implicit val strategy = Actor.strategy(executor)

  "actor" should {
    "execute code async" in {
      val latch = new CountDownLatch(1)
      val actor = Actor[Int]((i: Int) => latch.countDown())
      actor ! 1
      assertCountDown(latch, "Should process a message")
    }

    "catch code errors that can be handled" in {
      val latch = new CountDownLatch(1)
      val actor = Actor[Int]((i: Int) => 100 / i, (ex: Throwable) => latch.countDown())
      actor ! 0
      assertCountDown(latch, "Should catch an exception")
    }

    "exchange messages with another actor without loss" in {
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
      assertCountDown(latch, "Should process " + NumOfMessages + " messages")
    }
  }

  "actor strategy" should {
    "execute code async for different pools" in {
      val l = new CountDownLatch(2/*3*/)
      val f = (_: Int) => l.countDown()
      Actor(f) {
        import scala.concurrent.forkjoin._
        Actor.strategy(new ForkJoinPool(NumOfThreads))
      } ! 1
// TODO uncomment if Java 6 support will be discarded or add dependency on JSR166 classes compiled for Java 6
/*
      Actor(f) {
        Actor.strategy(new ForkJoinPool(NumOfThreads))
      } ! 1
*/
      Actor(f) {
        Actor.strategy(Executors.newFixedThreadPool(NumOfThreads))
      } ! 1
      assertCountDown(l, "Should execute on all pools")
    }

    "redirect unhandled errors to thread's uncaught exception handler of different pools" in {
      val l = new CountDownLatch(2/*3*/)
      val f = (_: Int) => throw new RuntimeException()
      val h = new java.lang.Thread.UncaughtExceptionHandler() {
        override def uncaughtException(t: Thread, e: Throwable): Unit = l.countDown()
      }
      Actor(f) {
        import scala.concurrent.forkjoin._
        Actor.strategy(new ForkJoinPool(NumOfThreads, new ForkJoinPool.ForkJoinWorkerThreadFactory() {
          override def newThread(pool: ForkJoinPool): ForkJoinWorkerThread = new ForkJoinWorkerThread(pool) {
            setUncaughtExceptionHandler(h)
          }
        }, null, true))
      } ! 1
// TODO uncomment if Java 6 support will be discarded or add dependency on JSR166 classes compiled for Java 6
/*
      Actor(f) {
        Actor.strategy(new ForkJoinPool(NumOfThreads, new ForkJoinPool.ForkJoinWorkerThreadFactory() {
          override def newThread(pool: ForkJoinPool): ForkJoinWorkerThread = new ForkJoinWorkerThread(pool) {
            setUncaughtExceptionHandler(h)
          }
        }, null, true))
      } ! 1
*/
      Actor(f) {
        Actor.strategy(new ThreadPoolExecutor(NumOfThreads, NumOfThreads, 0L, TimeUnit.MILLISECONDS,
          new LinkedBlockingQueue[Runnable], new ThreadFactory {
            override def newThread(r: Runnable): Thread = new Thread(r) {
              setUncaughtExceptionHandler(h)
            }
          }))
      } ! 1
      assertCountDown(l, "Should handle all uncaught exceptions")
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
