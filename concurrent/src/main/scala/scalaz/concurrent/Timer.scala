package scalaz
package concurrent

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import java.util.concurrent.locks.ReentrantReadWriteLock

import scalaz.syntax.either._

trait Timeout
object Timeout extends Timeout

case class Timer(timeoutTickMs: Int = 100, workerName: String = "TimeoutContextWorker") {
  val safeTickMs = if (timeoutTickMs > 5) timeoutTickMs else 5
  private[this] val futureNondeterminism = Nondeterminism[Future]
  private[this] val taskNondeterminism = Nondeterminism[Task]
  @volatile private[this] var continueRunning: Boolean = true
  @volatile private[this] var lastNow: Long = alignTimeResolution(System.currentTimeMillis)
  private[this] val lock = new ReentrantReadWriteLock()
  private[this] var futures: SortedMap[Long, List[() => Unit]] = SortedMap()
  private[this] val workerRunnable = new Runnable() {
    def run() {
      @tailrec
      def innerRun() {
        lastNow = alignTimeResolution(System.currentTimeMillis)
        // Deal with stuff to expire.
        futures.headOption match {
          case Some((time, _)) if (time <= lastNow) => {
            val expiredFutures: SortedMap[Long, List[() => Unit]] = withWrite{
              val (past, future) = futures.span(pair => pair._1 < lastNow)
              futures = future
              past
            }
            expireFutures(expiredFutures)
          }
          case _ => ()
        }
        // Should we keep running?
        if (continueRunning) {
          Thread.sleep(safeTickMs)
          innerRun()
        }
      }
      innerRun()
    }
  }
  private[this] val workerThread = new Thread(workerRunnable, workerName)
  workerThread.start()

  private[this] def expireFutures(futures: SortedMap[Long, List[() => Unit]]) {
    futures.foreach(vector => vector._2.foreach(call => call()))
  }

  def stop(expireImmediately: Boolean = false) {
    withWrite{
      continueRunning = false
      if (expireImmediately) {
        expireFutures(futures)
        futures = SortedMap()
      }
    }
  }

  private[this] def withWrite[T](expression: => T): T = {
    lock.writeLock().lock()
    try {
      expression
    } finally {
      lock.writeLock().unlock()
    }
  }

  private[this] def withRead[T](expression: => T): T = {
    lock.readLock().lock()
    try {
      expression
    } finally {
      lock.readLock().unlock()
    }
  }

  private[this] def alignTimeResolution(time: Long): Long = time / timeoutTickMs * timeoutTickMs

  def valueWait[T](value: T, waitMs: Long): Future[T] = {
    withRead{
      if (continueRunning) {
        val listen: (T => Unit) => Unit = callback => withWrite{
          val waitTime = alignTimeResolution(lastNow + (if (waitMs < 0) 0 else waitMs))
          val timedCallback = () => callback(value)
          // Lazy implementation for now.
          futures = futures + futures.get(waitTime).map(current => (waitTime, timedCallback :: current)).getOrElse((waitTime, List(timedCallback)))
        }
        Future.async(listen)
      } else {
        Future.now(value)
      }
    }
  }

  def withTimeout[T](future: Future[T], timeout: Long): Future[Timeout \/ T] = {
    val timeoutFuture = valueWait(Timeout, timeout)
    futureNondeterminism.choose(timeoutFuture, future).map(_.fold(_._1.left, _._2.right))
  }

  def withTimeout[T](task: Task[T], timeout: Long): Task[Timeout \/ T] = {
    val timeoutTask = new Task(valueWait(Timeout, timeout).map(_.right[Throwable]))
    taskNondeterminism.choose(timeoutTask, task).map(_.fold(_._1.left, _._2.right))
  }
}

object Timer {
  lazy val default = Timer()
}
