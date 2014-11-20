package scalaz
package concurrent

import java.util.concurrent._
import ConcurrentTest._

object ActorTest extends SpecLite {
  val NumOfMessages = 100000
  val NumOfThreads = 4

  "actor with sequential strategy" should {
    actorTests(NumOfMessages)(Strategy.Sequential)
  }

  "actor with default strategy" should {
    actorTests(NumOfMessages)(Strategy.DefaultStrategy)
  }

  "actor with executor strategy backed by Scala fork-join pool" should {
    actorTests(NumOfMessages)(Strategy.Executor(new scala.concurrent.forkjoin.ForkJoinPool()))
  }

  "actor with executor strategy backed by Java fork-join pool" should {
    actorTests(NumOfMessages)(Strategy.Executor(new ForkJoinPool()))
  }

  "actor with executor strategy backed by fixed thread pool" should {
    actorTests(NumOfMessages)(Strategy.Executor(Executors.newFixedThreadPool(NumOfThreads, Strategy.DefaultDaemonThreadFactory)))
  }

  "actor with naive strategy" should {
    actorTests(NumOfMessages / 1000)(Strategy.Naive)
  }

  "actor with Swing worker strategy" should {
    actorTests(NumOfMessages)(Strategy.SwingWorker)
  }

  "actor with Swing invoke later strategy" should {
    actorTests(NumOfMessages)(Strategy.SwingInvokeLater)
  }

  "actor with actor strategy backed by Scala fork-join pool" should {
    actorTests(NumOfMessages)(Actor.strategy(new scala.concurrent.forkjoin.ForkJoinPool()))
  }

  "actor with actor strategy backed by Java fork-join pool" should {
    actorTests(NumOfMessages)(Actor.strategy(new ForkJoinPool()))
  }

  "actor with actor strategy backed by fixed thread pool" should {
    actorTests(NumOfMessages)(Actor.strategy(Executors.newFixedThreadPool(NumOfThreads, Strategy.DefaultDaemonThreadFactory)))
  }

  def actorTests(n: Int)(implicit s: Strategy) = {
    "execute code async" in {
      val latch = new CountDownLatch(1)
      val actor = Actor[Int]((i: Int) => latch.countDown())
      actor ! 1
      assertCountDown(latch, "Should process a message")
    }

    "catch code errors that can be handled" in {
      val latch = new CountDownLatch(1)
      val actor = Actor[Int]((i: Int) => 1 / 0, (ex: Throwable) => latch.countDown())
      actor ! 1
      assertCountDown(latch, "Should catch an exception")
    }

    "exchange messages with another actor without loss" in {
      val latch = new CountDownLatch(n)
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
      actor1 ! n
      assertCountDown(latch, "Should exchange " + n + " messages")
    }

    "send messages to itself and process them" in {
      val latch = new CountDownLatch(1)
      var actor: Actor[Int] = null
      actor = Actor[Int] {
        (i: Int) =>
          if (i > 0) actor ! i - 1
          else latch.countDown()
      }
      actor ! n
      assertCountDown(latch, "Should send & process " + n + " messages")
    }

    "handle messages in order of sending by each thread" in {
      val latch = new CountDownLatch(n)
      val actor = countingDownActor(latch)
      for (j <- 1 to NumOfThreads) fork {
        for (i <- 1 to n / NumOfThreads) {
          actor !(j, i)
        }
      }
      assertCountDown(latch, "Should process " + n + " messages")
    }

    "redirect unhandled errors to uncaught exception handler of thread" in {
      val l = new CountDownLatch(1)
      val err = System.err
      try {
        System.setErr(new java.io.PrintStream(new java.io.OutputStream {
          override def write(b: Int): Unit = l.countDown()
        }))
        Actor((_: Int) => 1 / 0) ! 1
        assertCountDown(l, "Should print to System.err uncaught exception")
      } catch {
        case e: ArithmeticException if e.getMessage == "/ by zero" =>
          l.countDown() // for a sequential strategy
          assertCountDown(l, "Should print to System.err uncaught exception")
      } finally System.setErr(err)
    }
  }

  def countingDownActor(latch: CountDownLatch): Actor[(Int, Int)] = Actor[(Int, Int)] {
    val ms = collection.mutable.Map[Int, Int]()

    (m: (Int, Int)) =>
      val (j, i) = m
      if (ms.getOrElse(j, 0) + 1 == i) {
        ms.put(j, i)
        latch.countDown()
      }
  }
}
