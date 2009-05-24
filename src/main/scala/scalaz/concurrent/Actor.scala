package scalaz.concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue

sealed trait Effect[-A] {
  val effect: A => () => Unit
  val strategy: Strategy[Unit]

  def act(a: A) = strategy(effect(a))
}

sealed trait Actor[A] {
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  private def work = if (suspended.compareAndSet(!mbox.isEmpty, false)) effect act () else ()

  protected def selfish: Effect[A]

  def effect: Effect[Unit]

  def !(a: A) = if (mbox offer a) work else selfish act a
}

object Actor {
  def act[A](e: A => Unit)(implicit s: Strategy[Unit]) = new Effect[A] {
    val effect = (a: A) => () => e(a)
    val strategy = s
  }

  implicit def actFrom[A](implicit a: Effect[A], s: Strategy[Unit]) = (a.act(_))

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