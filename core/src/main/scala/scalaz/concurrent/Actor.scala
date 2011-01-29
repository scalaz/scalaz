package scalaz
package concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue
import Scalaz._
                  
sealed case class Actor[A](val e: A => Unit, val onError: Throwable => Unit = throw(_))(implicit val strategy: Strategy) { 
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  private def work = {
    val mt = mbox.isEmpty
    if (mt) () => ()
    else if (suspended.compareAndSet(!mt, false)) act ! (())
    else () => ()
  }

  val toEffect: Effect[A] = effect[A]((a) => this ! a)

  def !(a: A) = if (mbox offer a) work else toEffect ! a

  def apply(a: A) = this ! a
  
  private val act: Effect[Unit] = effect((u: Unit) => {
    var go = true
    var i = 0 
    while (go && i < 1000) {
      val m = mbox.poll
      if (m != null) try {
        e(m)
        i = i + 1
      } catch { case e => onError(e) }
      else {
        suspended.set(true)
        work
        go = false
      }
    }
    if (mbox.peek != null) act ! u else ()
  })
}

trait Actors {
  def actor[A](e: A => Unit, err: Throwable => Unit = throw(_))(implicit s: Strategy): Actor[A] = Actor[A](e,err)
  implicit def ActorFrom[A](a: Actor[A]): A => Unit = a ! _ 
}
