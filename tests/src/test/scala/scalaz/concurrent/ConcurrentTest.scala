package scalaz
package concurrent

import java.util.concurrent._
import org.scalacheck.Prop

object ConcurrentTest extends SpecLite{

  "Current test tools" should {
    "succeed on exhaused CountDownLatch" in {
      val latch = new CountDownLatch(1)
      latch.countDown
      assertCountDown(latch, "Should always be processed immediately")
    }

    "run forked code" in {
      val latch = new CountDownLatch(1)
      fork {
        latch.countDown
      }
      assertCountDown(latch, "Should be processed asynchronously")
    }

    "run test with timeout" in {
      (withTimeout(2000) {
        true
      })
    }

    "fail when timeout occurs" in {
      (withTimeout(100) {
        Thread.sleep(2000)
        ()
      }).mustThrowA[RuntimeException]
    }
  }

  def assertCountDown(latch: CountDownLatch, hint: String, timeout: Long = 1000) : Prop = {
    if (latch.await(timeout, TimeUnit.MILLISECONDS)) ()
    else sys.error("Failed to count down within " + timeout + " millis: " + hint)
  }

  def fork(f: => Unit) {
    new Thread {
      override def run() {
        f
      }
    }.start()
  }

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
}
