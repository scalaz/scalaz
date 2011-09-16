package scalaz
package concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import Scalaz._


sealed class Promise[A](implicit val strategy: Strategy) extends Function0[A] {
  import Promise._

  private val latch = new CountDownLatch(1)
  private val waiting = new ConcurrentLinkedQueue[A => Unit]
  private val errorHandlers = new ConcurrentLinkedQueue[Throwable => Unit]
  @volatile private var v: Promise.State[A] = Promise.Unfulfilled
  @volatile private var borked: Boolean = false
  private val e = actor[Signal[A]](_.eval, x => v.fulfill(throw x, this))

  def get = {
    latch.await
    v.get
  }

  def to(k: A => Unit): Unit = e ! new Cont(k, this)
  def errorTo(k: Throwable => Unit): Unit = e ! new Err(k, this)
  def fulfill(a: => A): Unit = e ! new Done(a, this)
  def fulfilled: Boolean = v.fulfilled
  def threw: Boolean = v.threw
  def broken = borked

  // out of band signal
  def break { 
    borked = true 
    e ! new Break(this)
  }

  def map[B](f: A => B) = flatMap(a => Promise(f(a)))
  def flatMap[B](f: A => Promise[B]) = {
    val r = new Promise[B]()
    errorTo (x => r fulfill (throw x))
    to(a => (try {
      val p = f(a)
      p errorTo (x => r fulfill (throw x))
      p
    } catch {
      case x => {
        r fulfill (throw x)
        (throw x) : Promise[B]
      }
    }) to effect[B](b => r fulfill b))
    r
  }
  def filter(p: A => Boolean): Promise[A] = {
    val r = new Promise[A]()
    to(a => promise(p(a)) to effect[Boolean](b => if (b) r fulfill a))
    r
  }

  def apply = get
  override def toString = "<promise>"

  def spec[B](f: A => B, actual: Promise[A])(implicit equality: Equal[A]): Promise[B] = {
    val speculation = this map f
    actual flatMap (a => this flatMap (g => if (a === g) speculation else {
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
    def fulfill[B>:A](a: => B, promise: Promise[B]): Unit
    val fulfilled: Boolean
    val threw: Boolean
    def break(promise: Promise[_]): Unit
  }
  private case class Thrown(e: Throwable) extends State[Nothing] {
    def get: Nothing = throw e
    def fulfill[B](a: => B, promise: Promise[B]) {
      // we could allow users to manually fulfill a thrown promise, 
      // but this would violate referential transparency. DENIED
      // Unfulfilled.fulfill(a, promise)
    }
    val fulfilled = true
    val threw = true
    def break(promise: Promise[_]) {}
  }
  private class Fulfilled[A](val get: A) extends State[A] {
    def fulfill[B>:A](a: => B, promise: Promise[B]) {}
    val fulfilled = true
    val threw = false
    def break(promise: Promise[_]) {}
  }
  private object Unfulfilled extends State[Nothing] {
    // the only way get gets here is if the promise is broken
    def get: Nothing = throw new BrokenException

    def fulfill[B](a: => B, promise: Promise[B]) {
      if (!promise.borked) {
        try { 
          promise.v = new Fulfilled(a)
        } catch { 
          case e : Throwable => {
            promise.v = new Thrown(e)
            val es = promise.errorHandlers
            while (!es.isEmpty) es.remove()(e)
            promise.latch.countDown
            promise.waiting.clear // kill teh hordes
            promise.errorHandlers.clear
          }
        }
        promise.latch.countDown
        val as = promise.waiting
        while (!as.isEmpty) as.remove()(a)
      }
    }
    val fulfilled = false
    val threw = false
    def break(promise: Promise[_]) {
      promise.latch.countDown // free the hordes
    }
  }
  
  sealed class BrokenException extends Exception 

  private abstract sealed class Signal[+A] {
    def eval: Unit
  }
  private class Done[+A](a: => A, promise: Promise[A]) extends Signal[A] {
    def eval {
      promise.v.fulfill(a, promise)
    }
  }
  private class Cont[+A](k: A => Unit, promise: Promise[A]) extends Signal[A] {
    def eval {
      if (promise.v.fulfilled) k(promise.v.get)
      else promise.waiting.offer(k)
    }
  }
  private class Break(promise: Promise[_]) extends Signal[Nothing] {
    def eval {
      promise.v.break(promise)
    }
  }
  private class Err[+A](k: Throwable => Unit, promise: Promise[A]) extends Signal[A] {
    def eval {
      promise.v match {
        case Thrown(e) => k(e)
        case _ => promise.errorHandlers.offer(k)
      }
    }
  } 
}
