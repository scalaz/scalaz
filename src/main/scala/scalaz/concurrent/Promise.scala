package scalaz.concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

sealed trait Promise[A] {
  private val latch = new CountDownLatch(1)
  private val waiting = new ConcurrentLinkedQueue[Actor[A]]
  @volatile private var v: Option[A] = None
  protected val actor: Actor[(Either[() => A, Actor[A]], Promise[A])]
  val strategy: Strategy[Unit]

  def get = {
    latch.await
    v.get
  }

  def to(a: Actor[A]) = actor ! (Right(a), this)
}

object Promise {
  private def mkPromise[A](implicit s: Strategy[Unit]) = new Promise[A] {
    val strategy = s
    val actor = Actor.actor[(Either[() => A, Actor[A]], Promise[A])]((p) => {
      val promise = p._2
      val as = promise.waiting
      p._1 match {
        case Left(l) => {
          val a = l()
          promise.v = Some(a)
          promise.latch.countDown
          while (!as.isEmpty) as.remove() ! a
        }
        case Right(r) => {
          if (promise.v.isEmpty) as.offer(r)
          else r ! promise.v.get
        }
      }
    })
  }

  def promise[A](a: => A)(implicit s: Strategy[Unit]) = {
    val p = mkPromise[A]
    p.actor ! ((Left(() => a), p))
    p
  }
}