package scalaz.concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import scalaz.Scalaz._
import scalaz.MA._
import scalaz.Cofunctor._
import Effect._

sealed trait Promise[A] {
  private val latch = new CountDownLatch(1)
  private val waiting = new ConcurrentLinkedQueue[A => Unit]
  @volatile private var v: Option[A] = None
  protected val e: Effect[(Either[() => A, A => Unit], Promise[A])]
  val strategy: Strategy[Unit]

  def get = {
    latch.await
    v.get
  }

  def to(a: A => Unit) = e ! ((Right(a), this))

  def bind[B](f: A => Promise[B]) = {
    val r = Promise.mkPromise[B](strategy)
    val ab = effect[B]((b: B) => r.e ! ((Left(() => b), r)))(strategy)
    to((a) => f(a).to(ab))
    r
  }
}

object Promise {
  private def mkPromise[A](implicit s: Strategy[Unit]) = new Promise[A] {
    val strategy = s
    val e = effect[(Either[() => A, A => Unit], Promise[A])]((p) => {
      val promise = p._2
      val as = promise.waiting
      p._1 match {
        case Left(l) => {
          val a = l()
          promise.v = Some(a)
          promise.latch.countDown
          while (!as.isEmpty) (as.remove())(a)
        }
        case Right(r) => {
          if (promise.v.isEmpty) as.offer(r)
          else r(promise.v.get)
        }
      }
    })
  }

  def promise[A](a: => A)(implicit s: Strategy[Unit]) = {
    val p = mkPromise[A]
    p.e ! ((Left(() => a), p))
    p
  }

  implicit def promiseFrom[A](implicit a: Promise[A]) = () => a.get
}