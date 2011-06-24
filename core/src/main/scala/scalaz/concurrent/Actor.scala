package scalaz
package concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue
import Scalaz._

sealed case class Actor[A](val e: A => Unit, val onError: Throwable => Unit = throw (_))(implicit val strategy: Strategy) {
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  private def work = {
    val mt = mbox.isEmpty
    if (mt) () => ()
    else if (suspended.compareAndSet(!mt, false)) act ! (())
    else () => ()
  }

  val toRun: Run[A] = run[A]((a) => this ! a)

  def !(a: A) = if (mbox offer a) work else toRun ! a

  def apply(a: A) = this ! a

  private val act: Run[Unit] = run((u: Unit) => {
    var go = true
    var i = 0
    while (go && i < 1000) {
      val m = mbox.poll
      if (m != null) try {
        e(m)
        i = i + 1
      } catch {
        case e => onError(e)
      }
      else {
        suspended.set(true)
        work
        go = false
      }
    }
    if (mbox.peek != null) act ! u else ()
  })
}

object Actor extends Actors

trait Actors {
  def actor[A](e: A => Unit, err: Throwable => Unit = throw (_))(implicit s: Strategy): Actor[A] =
    Actor[A](e, err)

  implicit def ActorContravariant: Contravariant[Actor] =
    new Contravariant[Actor] {
      def contramap[A, B](f: B => A) =
        r => actor[B]((b: B) => (r ! f(b))(), r.onError)(r.strategy)
    }

  implicit def ActorFrom[A](a: Actor[A]): A => Unit = a ! _
}
