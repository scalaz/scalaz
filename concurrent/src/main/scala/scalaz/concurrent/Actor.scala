package scalaz
package concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue

/**
 * Processes messages of type `A` sequentially. Messages are submitted to
 * the actor with the method `!`. Processing is typically performed asynchronously,
 * this is controlled by the provided `strategy`.
 *
 * @see scalaz.concurrent.Promise
 *
 * @param handler  The message handler
 * @param onError  Exception handler, called if the message handler throws any `Throwable`.
 * @param strategy Execution strategy, for example, a strategy that is backed by an `ExecutorService`
 * @tparam A       The type of messages accepted by this actor.
 */
final case class Actor[A](handler: A => Unit, onError: Throwable => Unit = throw(_))
                         (implicit val strategy: Strategy) {
  self =>

  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  val toEffect: Run[A] = Run[A]((a) => this ! a)

  /** Alias for `apply` */
  def !(a: A) {
    mbox offer a
    work
  }

  /** Pass the message `a` to the mailbox of this actor */
  def apply(a: A) {
    this ! a
  }

  def contramap[B](f: B => A): Actor[B] =
    Actor[B]((b: B) => (this ! f(b)), onError)(strategy)

  private def work {
    if (!mbox.isEmpty && suspended.compareAndSet(true, false))
      strategy(act)
  }

  private def act {
    var i = 0
    val batchSize = 1000
    while (i < batchSize) {
      val m = mbox.poll
      if (m != null) try {
        handler(m)
        i = i + 1
      } catch {
        case ex => onError(ex)
      }
      else i = batchSize
    }
    suspended.set(true)
    work
  }
}

object Actor extends ActorFunctions with ActorInstances

trait ActorInstances {
  implicit def actorContravariant: Contravariant[Actor] = new Contravariant[Actor] {
    def contramap[A, B](r: Actor[A])(f: (B) => A): Actor[B] = r contramap f
  }
}

trait ActorFunctions {
  def actor[A](e: A => Unit, err: Throwable => Unit = throw (_))(implicit s: Strategy): Actor[A] =
    Actor[A](e, err)

  implicit def ToFunctionFromActor[A](a: Actor[A]): A => Unit = a ! _
}
