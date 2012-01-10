package scalaz
package concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue

final case class Actor[A](e: A => Unit, onError: Throwable => Unit = throw(_))(implicit val strategy: Strategy) {
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  val toEffect: Run[A] = Run.run[A]((a) => this ! a)

  /** Alias for `apply` */
  def !(a: A) {
    mbox offer a
    work
  }

  /** Pass the message `a` to the mailbox of this actor */
  def apply(a: A) = this ! a

  def contramap[B](f: B => A): Actor[B] =
    Actor[B]((b: B) => (this ! f(b)), onError)(strategy)

  private def work =
    if (!mbox.isEmpty && suspended.compareAndSet(true, false)) act(())

  private val act: Run[Unit] = Run.run{(u: Unit) =>
    var i = 0
    while (i < 1000) {
      val m = mbox.poll
      if (m != null) try {
        e(m)
        i = i + 1
      } catch { case e => onError(e) }
      else i = 1000
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
