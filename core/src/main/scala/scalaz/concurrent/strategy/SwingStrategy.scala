package scalaz
package concurrent
package strategy

import javax.swing.{SwingUtilities, SwingWorker}
import java.util.concurrent.atomic.AtomicReference
import SwingUtilities.invokeAndWait

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
      () => {
        val ref = new AtomicReference[A]
        invokeAndWait(new Runnable {
          def run = ref.set(a())
        })
        ref.get
      }
    }
  }
}