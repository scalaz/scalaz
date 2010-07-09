package scalaz
package concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue
import Scalaz._
                  
sealed trait Actor[A] {
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
    val m = mbox.poll
    if (m != null) try {
      e(m)
      act ! u
    } catch { case e => onError(e) }
    else {
      suspended.set(true)
      work
    }
  })
  
  val e: A => Unit

  implicit val strategy: Strategy

  val onError: Throwable => Unit
}

trait Actors {
  def actor[A](err: Throwable => Unit, c: A => Unit)(implicit s: Strategy): Actor[A] = new {
    val e = c

    implicit val strategy = s

    val onError = err
  } with Actor[A]

  def actor[A](c: A => Unit)(implicit s: Strategy): Actor[A] = actor[A]((e: Throwable) => throw e, c): Actor[A]

  implicit def ActorFrom[A](a: Actor[A]): A => Unit = a ! _ 
}
