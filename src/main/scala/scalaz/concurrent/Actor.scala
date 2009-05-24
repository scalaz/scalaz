package scalaz.concurrent

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentLinkedQueue

sealed trait Actor[A] {
  private val suspended = new AtomicBoolean(true)
  private val mbox = new ConcurrentLinkedQueue[A]

  private def work = if (suspended.compareAndSet(!mbox.isEmpty, false)) act ! (()) else ()

  protected val selfish: Effect[A]

  protected val act: Effect[Unit]

  val e: A => Unit

  def !(a: A) = if (mbox offer a) work else selfish ! a
}

import Effect._

object Actor {
  def actor[A](c: A => Unit)(implicit s: Strategy[Unit]) = new Actor[A] {
    val act: Effect[Unit] = effect((u) => {
      c(mbox.remove)
      if (mbox.isEmpty) {
        suspended.set(true)
        work
      }
      else act ! u
    })

    val selfish = effect[A]((a) => this ! a)

    val e = c
  }

  implicit val ActorCofunctor = new Cofunctor[Actor] {
    def comap[A, B](r: Actor[A], f: B => A) = actor[B]((b) => r.act ! f(b))(r.act.strategy)
  }

  implicit def actorFrom[A](implicit a: Actor[A]): A => Unit = ((m) => a ! m)
}