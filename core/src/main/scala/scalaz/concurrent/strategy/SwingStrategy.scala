package scalaz
package concurrent
package strategy

import javax.swing.{SwingUtilities, SwingWorker}
import SwingUtilities.invokeLater
import java.util.concurrent.{Callable, FutureTask}

object SwingStrategy {

  /**
   * A strategy that evaluates its arguments using the pool of Swing worker threads.
   */
  implicit def workerStrategy[A]: Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = {
      val worker = new SwingWorker[A, Unit] {
        def doInBackground = a()
      }
      worker.execute
      () => worker.get
    }
  }

  /**
   * A strategy that evaluates its arguments on the Swing Event Dispatching thread.
   */
  implicit def eventDispatchingThreadStrategy[A]: Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = {
      val task = new FutureTask[A](new Callable[A] {
        def call = a()
      })
      invokeLater(task)
      () => task.get
    }
  }
}