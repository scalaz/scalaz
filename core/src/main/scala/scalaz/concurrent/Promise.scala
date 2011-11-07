package scalaz
package concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import Scalaz._

/** Represents an expression that is evaluated asynchronously, according to some
    evaluation strategy. */
sealed class Promise[A](implicit val strategy: Strategy) extends Function0[A] {
  import Promise._

  private val latch = new CountDownLatch(1)
  private val waiting = new ConcurrentLinkedQueue[A => Unit]
  private val errorHandlers = new ConcurrentLinkedQueue[Throwable => Unit]
  @volatile private var v: Promise.State[A] = Promise.Unfulfilled
  @volatile private var borked: Boolean = false
  private val e = actor[Signal[A]](_.eval, x => v.fulfill(throw x, this))

  /** Syncrhonously blocks until the value is ready, then returns it.
    * WARNING: May block indefinitely. A kitten dies every time you call this method. */
  def get = {
    latch.await
    v.get
  }

  /** Registers the given actor or effect as a continuation receiving the promised value. */
  def to(k: A => Unit): Unit = e ! new Cont(k, this)

  /** Registers the given error handler as a continuation receiving errors thrown by the evaluation. */
  def errorTo(k: Throwable => Unit): Unit = e ! new Err(k, this)

  /** Sets the value of this promise, out of band.
    * WARNING: Once the value of a promise is set, it is forever immutable. */
  def fulfill(a: => A): Unit = e ! new Done(a, this)

  /** Indicates whether the evaluation of this promise has finished. */
  def fulfilled: Boolean = v.fulfilled

  /** Indicates whether the evaluation of this promise threw an error. */
  def threw: Boolean = v.threw

  /** Indicates whether the promise has been broken. A broken promise is never fulfilled. */
  def broken = borked

  /** Breaks the promise. After this method finishes, any registered continuation will not receive the value.
    * If evaluation has not yet started, it will never start. */
  def break { 
    borked = true 
    e ! new Break(this)
  }

  /** Registers the given function as a continuation, returning a promise of the result. */
  def map[B](f: A => B) = flatMap(a => Promise(f(a)))

  /** Registers the given function as a continuation, returning the result of that function. */
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

  /** Returns a promise that is only ever fulfilled if the value of this promise matches the given predicate. */
  def filter(p: A => Boolean): Promise[A] = {
    val r = new Promise[A]()
    to(a => promise(p(a)) to effect[Boolean](b => if (b) r fulfill a))
    r
  }

  /** Syncrhonously blocks until the value is ready, then returns it.
    * WARNING: May block indefinitely. A kitten dies every time you call this method. */
  def apply = get

  override def toString = "<promise>"

  /** Speculative concurrency. In the expression `guess.spec(f, actual)` the value of 
    * `guess` is an inexpensive guess at the value and `actual` is the actual value,
    * usually a more expensive computation. The function `f` is ''speculatively'' applied
    * to the `guess`. Once the `actual` value is available, it's compared with the `guess`.
    * If the `guess` was correct, we will have already applied `f` to the correct value,
    * saving time. If the guess was incorrect, then `f` is applied to the value of `actual`. */
  def spec[B](f: A => B, actual: Promise[A])(implicit equality: Equal[A]): Promise[B] = {
    val speculation = this map f
    actual flatMap (a => this flatMap (g => if (a === g) speculation else {
      speculation.break
      promise(f(a))
    }))
  }
}

trait Promises {
  /** Evaluate the given expression asynchronously given the strategy. */
  def promise[A](a: => A)(implicit s: Strategy): Promise[A] = Promise(a)

  /** Construct an empty promise that you must remember to fulfill later. */
  def emptyPromise[A](implicit s: Strategy) = new Promise[A]()(s)
}

object Promise {
  /** Evaluate the given expression asynchronously given the strategy. */
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
