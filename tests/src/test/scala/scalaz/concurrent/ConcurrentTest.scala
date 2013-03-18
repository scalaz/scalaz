package scalaz
package concurrent

import scalaz.Spec
import java.util.concurrent._
import org.specs2.execute.{Success, Failure, Result}

class ConcurrentTest extends Spec{
  import ConcurrentTest._

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
        Success()
      }).isSuccess must_== true
    }

    "fail when timeout occurs" in {
      (withTimeout(100) {
        Thread.sleep(2000)
        Success()
      }).isSuccess must_== false
    }
  }
}

object ConcurrentTest {
  def assertCountDown(latch: CountDownLatch, hint: String, timeout: Long = 1000) : Result = {
    if (latch.await(timeout, TimeUnit.MILLISECONDS)) Success()
    else Failure("Failed to count down within " + timeout + " millis: " + hint)
  }

  def fork(f: => Unit) {
    new Thread {
      override def run() {
        f
      }
    }.start()
  }

  def withTimeout(timeout: Long)(test: => Result): Result = {
    val latch = new CountDownLatch(1)
    @volatile var result: Result = null
    fork {
      result = test
      latch.countDown
    }
    if (latch.await(timeout, TimeUnit.MILLISECONDS)) result
    else Failure("Timeout occured, possible deadlock.")
  }
}
