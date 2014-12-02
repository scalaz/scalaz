package scalaz
package concurrent

import java.util.concurrent._

object StrategyTest extends SpecLite {
  val NumOfIterations = 10000
  val NumOfThreads = 4

  "the sequential strategy" should {
    strategyTests(Strategy.Sequential)
  }

  "the default strategy" should {
    strategyTests(Strategy.DefaultStrategy)
  }

  "an executor strategy backed by Scala fork-join pool" should {
    recursiveStrategyTests(NumOfIterations, Strategy.Executor(new scala.concurrent.forkjoin.ForkJoinPool()))
  }

  "an executor strategy backed by Java fork-join pool with FIFO mode" should {
    recursiveStrategyTests(NumOfIterations, Strategy.Executor(Strategy.DefaultFIFOForkJoinPool))
  }

  "an executor strategy backed by Java fork-join pool with LIFO mode" should {
    recursiveStrategyTests(NumOfIterations, Strategy.Executor(Strategy.DefaultLIFOForkJoinPool))
  }

  "an executor strategy backed by fixed thread pool" should {
    strategyTests(Strategy.Executor(Executors.newFixedThreadPool(NumOfThreads, Strategy.DefaultDaemonThreadFactory)))
  }

  "the id strategy" should {
    recursiveStrategyTests(NumOfIterations, Strategy.Id)
  }

  "the naive strategy" should {
    recursiveStrategyTests(NumOfIterations / 100, Strategy.Naive)
  }

  "the Swing worker strategy" should {
    strategyTests(Strategy.SwingWorker)
  }

  "the Swing invoke later strategy" should {
    strategyTests(Strategy.SwingInvokeLater)
  }

  def strategyTests(s: Strategy) = {
    "evaluate result async" in {
      val f = s(1 + 1)
      f() must_== 2
    }

    "catch code errors that can be handled" in {
      try {
        val f = s(1 / 0)
        if (s eq Strategy.Id) f().mustThrowA[ArithmeticException]
        else f().mustThrowA[ExecutionException]
      } catch {
        case _: ArithmeticException => s must_== Strategy.Sequential
      }
    }
  }

  def recursiveStrategyTests(n: Int, s: Strategy) = {
    strategyTests(s)

    "be called recursively for parallel algorithms" in {
      def factorial(n: Int): BigInt = {
        def rangeProduct(n1: Int, n2: Int): BigInt = {
          val d = n2 - n1
          d match {
            case 0 => BigInt(n1)
            case 1 => BigInt(n1.toLong * n2)
            case 2 => BigInt(n1.toLong * (n1 + 1)) * n2
            case 3 => BigInt(n1.toLong * (n1 + 1)) * BigInt(n2.toLong * (n2 - 1))
            case _ =>
              val f = s(rangeProduct(n1, n1 + (d >> 1)))
              rangeProduct(n1 + (d >> 1) + 1, n2) * f()
          }
        }

        rangeProduct(1, n)
      }

      factorial(n) must_== (BigInt(1) to BigInt(n)).product
    }
  }
}
