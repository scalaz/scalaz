package scalaz
package concurrent

import Scalaz._

/**
 * Evaluate an expression in some specific manner. A typical strategy will schedule asynchronous
 * evaluation and return a function that, when called, will block until the result is ready.
 */
trait Strategy {
  def apply[A](a: => A): () => A
}

object Strategy {
  /**
   * A strategy that evaluates its argument in the current thread.
   */
  implicit object Sequential extends Strategy {
    def apply[A](a: => A) = {
      val v = a
      () => v
    }
  }

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
  implicit object Id extends Strategy {
    def apply[A](a: => A) = () => a
  }

  /**
   * A simple strategy that spawns a new thread for every evaluation.
   */
  implicit object Naive extends Strategy {
    import scala.concurrent.ops.future
    def apply[A](a: => A) = future {a}
  }

  /**
   * A strategy that evaluates its arguments using the pool of Swing worker threads.
   */
  implicit object SwingWorkerStrategy extends Strategy {
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
  implicit object EventDispatchingThreadStrategy extends Strategy {
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
