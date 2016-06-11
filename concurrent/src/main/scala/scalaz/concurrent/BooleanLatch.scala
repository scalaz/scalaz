package scalaz.concurrent

import java.util.concurrent._
import locks._

trait BooleanLatch {
  def release(): Boolean
  def await(): Unit
}

object BooleanLatch {
  def apply() = new BooleanLatch {
    val sync = new AbstractQueuedSynchronizer {
      val RELEASED = 0
      val UNAVAILABLE = -1

      setState(UNAVAILABLE)

      def released = getState == RELEASED
      def unavailable = getState == UNAVAILABLE

      override def tryAcquire(ignore: Int) =
        if (!released) false
        else compareAndSetState(RELEASED, UNAVAILABLE)

      override def tryRelease(ignore: Int) = {
        if (unavailable) setState(RELEASED)
        true
      }
    }

    override def release() = sync release 0
    override def await() = sync acquireInterruptibly 0
  }
}
