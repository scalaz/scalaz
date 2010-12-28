package scalaz
package concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import Scalaz._


sealed class Promise[A](implicit val strategy: Strategy) extends Function0[A] {
  import Promise._

  private val latch = new CountDownLatch(1)
  private val waiting = new ConcurrentLinkedQueue[A => Unit]
  @volatile private var v: Promise.State[A] = Promise.Unfulfilled
  @volatile private var borked: Boolean = false
  protected val e = actor((a: Signal[A]) => a.eval)

  def get = {
    latch.await
    v.get
  }

  def to(k: A => Unit) = e ! new Cont(k, this)
  def fulfill(a: => A) = e ! new Done(a, this)
  def fulfilled: Boolean = v.fulfilled
  def broken = borked

  // out of band signal
  def break { borked = true }

  def map[B](f: A => B) = flatMap(a => Promise(f(a)))
  def flatMap[B](f: A => Promise[B]) = {
    val r = new Promise[B]()
    to(a => f(a) to effect[B](b => r fulfill b))
    r
  }

  def apply = get
  override def toString = "<promise>"

  def spec[B](f: A => B, actual: Promise[A]): Promise[B] = {
    val speculation = this map f
    actual flatMap (a => this flatMap (g => if (a == g) speculation else {
      speculation.break
      promise(f(a))
    }))
  }
}

trait Promises {
  def promise[A](a: => A)(implicit s: Strategy): Promise[A] = Promise(a) 
}

object Promise {
  def apply[A](a: => A)(implicit s: Strategy): Promise[A] = { 
    val p = new Promise[A]()(s)
    p.e ! new Done(a, p)
    p
  }

  private sealed abstract class State[+A] {
    def get: A
    def fulfill[B>:A](a: B, promise: Promise[B]): Unit
    def fulfilled: Boolean
  }
  private class Thrown(e: Throwable) extends State[Nothing] {
    def get: Nothing = throw e
    def fulfill[B](a: B, promise: Promise[B]) = throw new AlreadyFulfilledException
    val fulfilled = true
  }
  private class Fulfilled[A](val get: A) extends State[A] {
    def fulfill[B>:A](a: B, promise: Promise[B]) = throw new AlreadyFulfilledException
    val fulfilled = true
  }
  private object Unfulfilled extends State[Nothing] {
    def get: Nothing = throw new UnfulfilledException
    def fulfill[B](a: B, promise: Promise[B]) {
      promise.v = new Fulfilled(a)
      promise.latch.countDown
      val as = promise.waiting
      while (!as.isEmpty) (as.remove())(a)
    }
    val fulfilled = false
  }
  
  sealed class UnfulfilledException extends Exception 
  sealed class AlreadyFulfilledException extends Exception


  private abstract sealed class Signal[+A] {
    def eval: Unit
  }
  private class Done[+A](a: => A, promise: Promise[A]) extends Signal[A] {
    def eval {
      if (!promise.borked) 
        promise.v.fulfill(a, promise)
    }
  }
  private class Cont[+A](k: A => Unit, promise: Promise[A]) extends Signal[A] {
    def eval {
      if (promise.v.fulfilled) k(promise.v.get)
      else promise.waiting.offer(k)
    }
  }
}
