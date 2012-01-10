package scalaz
package concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue
import Scalaz._
                  
final case class Actor[A](e: A => Unit, onError: Throwable => Unit = throw(_))(implicit val strategy: Strategy) {
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  private def work =
    if (!mbox.isEmpty && suspended.compareAndSet(true, false)) act(())

  val toEffect: Effect[A] = effect[A]((a) => this ! a)

  def !(a: A) {
    mbox offer a
    work
  }

  def apply(a: A) = this ! a
  
  private val act: Effect[Unit] = effect {(u: Unit) =>
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

trait Actors {
  def actor[A](e: A => Unit, err: Throwable => Unit = throw(_))(implicit s: Strategy): Actor[A] = Actor[A](e,err)
  implicit def ActorFrom[A](a: Actor[A]): A => Unit = a ! _ 
}
