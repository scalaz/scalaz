package scalaz
package concurrent

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

/**
 * Processes messages of type `A`, one at a time. Messages are submitted to
 * the actor with the method `!`. Processing is typically performed asynchronously,
 * this is controlled by the provided `strategy`.
 *
 * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it
 * mutates is guaranteed to be visible by the `handler` when it processes the next message, even if
 * the `strategy` runs the invocations of `handler` on separate threads. This is achieved because
 * the `Actor` reads a volatile memory location before entering its event loop, and writes to the same
 * location before suspending.
 *
 * Implementation based on non-intrusive MPSC node-based queue, described by Dmitriy Vyukov:
 * [[http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue]]
 *
 * @param handler  The message handler
 * @param onError  Exception handler, called if the message handler throws any `Throwable`.
 * @param strategy Execution strategy, for example, a strategy that is backed by an `ExecutorService`
 * @tparam A       The type of messages accepted by this actor.
 */
final case class Actor[A](handler: A => Unit, onError: Throwable => Unit = Actor.rethrow)
                         (implicit val strategy: Strategy) {
  private val head = new AtomicReference[Node[A]]

  def toEffect: Run[A] = Run[A](a => this ! a)

  /** Alias for `apply` */
  def !(a: A): Unit = {
    val n = new Node(a)
    val h = head.getAndSet(n)
    if (h ne null) h.lazySet(n)
    else schedule(n)
  }

  /** Pass the message `a` to the mailbox of this actor */
  def apply(a: A): Unit = this ! a

  def contramap[B](f: B => A): Actor[B] = new Actor[B](b => this ! f(b), onError)(strategy)

  private def schedule(n: Node[A]): Unit = strategy(act(n))

  @annotation.tailrec
  private def act(n: Node[A], i: Int = 1024, f: A => Unit = handler): Unit = {
    try f(n.a) catch {
      case ex: Throwable => onError(ex)
    }
    val n2 = n.get
    if (n2 eq null) scheduleLastTry(n)
    else if (i == 0) schedule(n2)
    else act(n2, i - 1, f)
  }

  private def scheduleLastTry(n: Node[A]): Unit = strategy(lastTry(n))

  private def lastTry(n: Node[A]): Unit = if (!head.compareAndSet(n, null)) act(next(n))

  @annotation.tailrec
  private def next(n: Node[A]): Node[A] = {
    val n2 = n.get
    if (n2 ne null) n2
    else next(n)
  }
}

private class Node[A](val a: A) extends AtomicReference[Node[A]]

object Actor extends ActorInstances with ActorFunctions

sealed abstract class ActorInstances {
  implicit val actorContravariant: Contravariant[Actor] = new Contravariant[Actor] {
    def contramap[A, B](r: Actor[A])(f: B => A): Actor[B] = r contramap f
  }
}

trait ActorFunctions {
  def actor[A](handler: A => Unit, onError: Throwable => Unit = rethrow)
              (implicit s: Strategy): Actor[A] = new Actor[A](handler, onError)(s)

  implicit def ToFunctionFromActor[A](a: Actor[A]): A => Unit = a ! _

  val rethrow: Throwable => Unit = {
    case _: InterruptedException => Thread.currentThread.interrupt()
    case e =>
      val t = Thread.currentThread
      val h = t.getUncaughtExceptionHandler
      if (h ne null) h.uncaughtException(t, e)
      throw e
  }
  /**
   * Creates a strategy that optimized for actors.
   * WARNING: This strategy cannot be used for evaluation of values.
   *
   * Implementation based on improvements committed to Akka by Viktor Klang:
   * https://github.com/akka/akka/pull/16152
   *
   * @param s an executor service instance
   * @return a strategy for actors
   */
  def strategy(implicit s: ExecutorService) = s match {
    case p: scala.concurrent.forkjoin.ForkJoinPool => new Strategy {

      import scala.concurrent.forkjoin.ForkJoinTask

      def apply[A](a: => A): () => A = {
        val t = new ForkJoinTask[Unit] {
          def getRawResult: Unit = ()

          def setRawResult(unit: Unit): Unit = ()

          def exec(): Boolean = {
            a
            false
          }
        }
        if (ForkJoinTask.getPool eq p) t.fork()
        else p.execute(t)
        null
      }
    }
    case p: ForkJoinPool => new Strategy {
      def apply[A](a: => A): () => A = {
        val t = new ForkJoinTask[Unit] {
          def getRawResult: Unit = ()

          def setRawResult(unit: Unit): Unit = ()

          def exec(): Boolean = {
            a
            false
          }
        }
        if (ForkJoinTask.getPool eq p) t.fork()
        else p.execute(t)
        null
      }
    }
    case p => new Strategy {
      def apply[A](a: => A): () => A = {
        p.execute(new Runnable {
          def run(): Unit = a
        })
        null
      }
    }
  }
}
