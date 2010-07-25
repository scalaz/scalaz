package scalaz
package concurrent

import Scalaz._
import java.util.concurrent.{ExecutorService, ThreadFactory, Executors}

/**
 * Evaluate an expression in some specific manner. A typical strategy will schedule asynchronous
 * evaluation and return a function that, when called, will block until the result is ready.
 */
trait Strategy {
  def apply[A](a: => A): () => A
}

abstract class StrategyLow {
  /**
   * A strategy that evaluates its argument in the current thread.
   */
  implicit val Sequential: Strategy = new Strategy {
    def apply[A](a: => A) = {
      val v = a
      () => v
    }
  }

  import java.util.concurrent.ExecutorService

  /**
   * A strategy that evaluates its arguments using an implicit ExecutorService.
   */
  implicit def Executor(implicit s: ExecutorService) = new Strategy {
    import java.util.concurrent.Callable
    def apply[A](a: => A) = {
      val fut = s.submit(new Callable[A] {
        def call = a
      })
      () => fut.get
    }
  }

  /**
   * A strategy that performs no evaluation of its argument.
   */
  implicit val Id: Strategy = new Strategy {
    def apply[A](a: => A) = () => a
  }

  /**
   * A simple strategy that spawns a new thread for every evaluation.
   */
  implicit val Naive: Strategy = new Strategy {
    import scala.concurrent.ops.future
    def apply[A](a: => A) = future {a}
  }

  /**
   * A strategy that evaluates its arguments using the pool of Swing worker threads.
   */
  implicit val SwingWorker: Strategy = new Strategy {
    import javax.swing.SwingWorker
    def apply[A](a: => A) = {
      val worker = new SwingWorker[A, Unit] {
        def doInBackground = a
      }
      worker.execute
      () => worker.get
    }
  }

  /**
   * A strategy that evaluates its arguments on the Swing Event Dispatching thread.
   */
  implicit val SwingInvokeLater: Strategy = new Strategy {
    import javax.swing.SwingUtilities
    import SwingUtilities.invokeLater
    import java.util.concurrent.{Callable, FutureTask}
    def apply[A](a: => A) = {
      val task = new FutureTask[A](new Callable[A] {
        def call = a
      })
      invokeLater(task)
      () => task.get
    }
  }
}

object Strategy extends StrategyLow {
  /**
   * The default executor service is a fixed thread pool with N daemon threads,
   * where N is equal to the number of available processors.
   */
  lazy val DefaultExecutorService: ExecutorService = {
    import Executors._
    newFixedThreadPool(Runtime.getRuntime.availableProcessors, new ThreadFactory {
      def newThread(r: Runnable) = {
        val t = defaultThreadFactory.newThread(r)
        t.setDaemon(true)
        t
      }
    })
  }

  /**
   * A strategy that executes its arguments on {@Strategy#DefaultExecutorService}.
   */
  implicit lazy val DefaultStrategy: Strategy = Executor(DefaultExecutorService)
}