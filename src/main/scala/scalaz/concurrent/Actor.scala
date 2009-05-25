package scalaz.concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue

sealed trait Actor[A] extends (A => Unit) {
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  private def work = if (suspended.compareAndSet(!mbox.isEmpty, false)) act ! (()) else () => ()

  val toEffect: Effect[A]

  protected val act: Effect[Unit]

  val e: A => Unit

  def !(a: A) = if (mbox offer a) work else toEffect ! a

  val strategy: Strategy[Unit]

  val onError: Throwable => Unit

  def apply(a: A) = this ! a
}

import Effect._

object Actor {
  def actor[A](err: Throwable => Unit, c: A => Unit)(implicit s: Strategy[Unit]): Actor[A] = new Actor[A] {
    val act: Effect[Unit] = effect((u) => {
      try {c(mbox.remove())} catch {
        case e => {
          err(e)
          act ! (())
        }
      }
      if (mbox.isEmpty) {
        suspended.set(true)
        work
      }
      else act ! u
    })

    val toEffect = effect[A]((a) => this ! a)

    val e = c

    val strategy = s

    val onError = err
  }

  def actor[A](c: A => Unit)(implicit s: Strategy[Unit]): Actor[A] = actor[A]((e: Throwable) => throw e, c)
}