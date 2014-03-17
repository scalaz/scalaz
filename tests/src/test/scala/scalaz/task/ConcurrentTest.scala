package scalaz
package task

import java.util.concurrent._

object ConcurrentTest {

  final class WithTimeout(timeout: Long) {
    def apply[A](test: => A): A = {
      val latch = new CountDownLatch(1)
      @volatile var result: A = null.asInstanceOf[A]
      fork {
        result = test
        latch.countDown
      }
      if (latch.await(timeout, TimeUnit.MILLISECONDS)) result
      else sys.error("Timeout occured, possible deadlock.")
    }
  }

  def withTimeout(timeout: Long) = new WithTimeout(timeout)

  def fork(f: => Unit) {
    new Thread {
      override def run() {
        f
      }
    }.start()
  }

}
