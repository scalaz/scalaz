package scalaz.concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue

sealed trait Act[-A] {
  protected val effect: A => () => Unit

  protected val strategy: Strategy[Unit]

  def act(a: A) = strategy(effect(a))
}

sealed trait Actor[A] {
  private val suspended: AtomicBoolean = new AtomicBoolean(true)
  private val mbox: ConcurrentLinkedQueue[A] = new ConcurrentLinkedQueue[A]

  def effect: Act[Unit]

  protected def selfish: Act[A]

  protected def work = if (suspended.compareAndSet(!mbox.isEmpty, false)) effect.act(()) else () => ()

  def !(a: A) = if (mbox.offer(a)) work else selfish.act(a)
}

object Actor {
  def act[A](e: A => Unit)(implicit s: Strategy[Unit]) = new Act[A] {
    val effect = (a: A) => () => e(a)
    val strategy = s
  }

  implicit def actFrom[A](implicit a: Act[A], s: Strategy[Unit]) = (a.act(_))

  def actor[A](e: A => Unit)(implicit s: Strategy[Unit]) = new Actor[A] {
    def effect = act((u) => {
      e(mbox.remove)
      if (mbox.isEmpty) {
        suspended.set(true)
        work
      }
      else effect.act(u)
    })

    def selfish = act((a) => this ! a)
  }
}