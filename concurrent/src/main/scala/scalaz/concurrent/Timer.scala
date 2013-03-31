package scalaz
package concurrent

import scalaz._
import Scalaz._
import scala.annotation.tailrec
import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.collection.immutable.SortedMap

trait Timeout
object Timeout extends Timeout

object Timer {
  private[this] val futureVectorSemigroup = Semigroup[Vector[() => Unit]]
  implicit val futuresMapMonoid: Monoid[SortedMap[Long, Vector[() => Unit]]] = new Monoid[SortedMap[Long, Vector[() => Unit]]] {
    def zero = SortedMap[Long, Vector[() => Unit]]()
    def append(m1: SortedMap[Long, Vector[() => Unit]], m2: => SortedMap[Long, Vector[() => Unit]]) = {
      // Eagerly consume m2 as the value is used more than once.
      val m2Instance: SortedMap[Long, Vector[() => Unit]] = m2
      // semigroups are not commutative, so order may matter.
      val (from, to, semigroup) = {
        if (m1.size > m2Instance.size) (m2Instance, m1, futureVectorSemigroup.append(_: Vector[() => Unit], _: Vector[() => Unit]))
        else (m1, m2Instance, (futureVectorSemigroup.append(_: Vector[() => Unit], _: Vector[() => Unit])).flip)
      }

      from.foldLeft(to) {
        case (to, (k, v)) => to + (k -> to.get(k).map(semigroup(_, v)).getOrElse(v))
      }
    }
  }
}

case class Timer(timeoutTickMs: Int = 100, workerName: String = "TimeoutContextWorker") {
  import Timer._
  val safeTickMs = if (timeoutTickMs > 0) timeoutTickMs else 1
  private[this] val futureNondeterminism = Nondeterminism[Future]
  private[this] val taskNondeterminism = Nondeterminism[Task]
  @volatile private[this] var continueRunning: Boolean = true
  @volatile private[this] var lastNow: Long = System.currentTimeMillis
  private[this] val lock = new ReentrantReadWriteLock()
  private[this] var futures: SortedMap[Long, Vector[() => Unit]] = SortedMap()
  private[this] val workerRunnable = new Runnable() {
    def run() {
      @tailrec
      def innerRun() {
        // Deal with stuff to expire.
        lastNow = System.currentTimeMillis
        futures.headOption match {
          case Some((time, _)) if (time < lastNow) => {
            val expiredFutures: SortedMap[Long, Vector[() => Unit]] = withWrite{
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

  private[this] def expireFutures(futures: SortedMap[Long, Vector[() => Unit]]) {
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
  
  def valueWait[T](value: T, waitMs: Long): Future[T] = {
    withRead{
      if (continueRunning) {
        val listen: (T => Unit) => Unit = callback => withWrite{
          val waitTime = lastNow + (if (waitMs < 0) 0 else waitMs / timeoutTickMs * timeoutTickMs)
          val timedCallback = () => callback(value)
          // Lazy implementation for now.
          futures = futures |+| SortedMap(waitTime -> Vector(timedCallback))
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

