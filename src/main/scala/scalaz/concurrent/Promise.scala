package scalaz.concurrent

import java.util.ArrayDeque
import java.util.concurrent.CountDownLatch

sealed trait Promise[A] {
  private val latch = new CountDownLatch(1)
  private val waiting = new ArrayDeque[Actor[A]]
  @volatile private var v: Option[A] = None
  protected val actor: Actor[(Either[() => A, Actor[A]], Promise[A])]
  val strategy: Strategy[Unit]

  def get = {
    latch.await
    v.get
  }
}

import Actor._

object Promise {
  private def mkPromise[A](implicit s: Strategy[Unit]) = {
    val q = actor[(Either[() => A, Actor[A]], Promise[A])]((p) => {
      val fst = p._1
      val snd = p._2
      val as = snd.waiting
      if (fst.isLeft) {
        val a = fst.left.get()
        snd.v = Some(a)
        snd.latch.countDown
        while (!as.isEmpty) as.removeFirst ! a
      }
      else if (snd.v.isEmpty) as.offerLast(fst.right.get)
      else fst.right.get ! snd.v.get
    })
    new Promise[A] {
      val strategy = s
      val actor = q
    }
  }

  def promise[A](a: => A)(implicit s: Strategy[Unit]) = {
    val p = mkPromise[A]
    p.actor ! ((Left(() => a), p))
    p
  }
}