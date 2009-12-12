package scalaz
package concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue
import Scalaz._
                  
sealed trait Actor[A] {
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  private def work = if (suspended.compareAndSet(!mbox.isEmpty, false)) act ! (()) else () => ()

  val toEffect: Effect[A] = effect[A]((a) => this ! a)(strategy)

  def !(a: A) = if (mbox offer a) work else toEffect ! a

  def apply(a: A) = this ! a
  
  private val act: Effect[Unit] = effect((u: Unit) => {
    val m = mbox.remove()
    try {e(m)} catch {
      case e => onError(e)
    }
    if (mbox.isEmpty) {
      suspended.set(true)
      work
    }
    else act ! u
  })(strategy)
  
  val e: A => Unit

  val strategy: Strategy[Unit]

  val onError: Throwable => Unit
}

trait Actors {
  def actor[A](err: Throwable => Unit, c: A => Unit)(implicit s: Strategy[Unit]): Actor[A] = new Actor[A] {
    val e = c

    val strategy = s

    val onError = err
  }

  def actor[A](c: A => Unit)(implicit s: Strategy[Unit]): Actor[A] = actor[A]((e: Throwable) => throw e, c)

  implicit def ActorFrom[A](a: Actor[A]): A => Unit = a ! _ 
}
