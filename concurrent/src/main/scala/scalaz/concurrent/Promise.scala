package scalaz
package concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

sealed trait Promise[A] {

  import Promise._
  import Actor._
  import Run._

  implicit val strategy: Strategy
  private val latch = new CountDownLatch(1)
  private val waiting = new ConcurrentLinkedQueue[A => Unit]
  @volatile private var v: Promise.State[A] = Promise.Unfulfilled
  @volatile private var borked: Boolean = false
  lazy private[concurrent] val e = actor[Signal[A]](_.eval)

  def get = {
    latch.await()
    v.get
  }

  def to(k: A => Unit): Unit = e ! new Cont(k, this)

  def fulfill(a: => A): Unit = e ! new Done(a, this)

  def fulfilled: Boolean = v.fulfilled

  def threw: Boolean = v.threw

  def broken = borked

  // out of band signal
  def break {
    borked = true
    e ! new Break(this)
  }

  def map[B](f: A => B) =
    flatMap(a => Promise.promise(f(a)))

  def flatMap[B](f: A => Promise[B]) = {
    val r = new Promise[B] {
      implicit val strategy = Promise.this.strategy
    }
    to(a => f(a) to run[B](b => r fulfill b))
    r
  }

  def filter(p: A => Boolean): Promise[A] = {
    val r = new Promise[A] {
      implicit val strategy = Promise.this.strategy
    }
    to(a => promise(p(a)) to run[Boolean](b => if (b) r fulfill a))
    r
  }

  def spec[B](f: A => B, actual: Promise[A])(implicit equality: Equal[A]): Promise[B] = {
    val speculation = this map f
    actual flatMap (a => this flatMap (g => if (equality.equal(a, g)) speculation
    else {
      speculation.break
      promise(f(a))
    }))
  }
}

object Promise extends PromiseFunctions with PromiseInstances {
  def apply[A](a: => A)(implicit s: Strategy): Promise[A] =
    promise(a)

  protected sealed abstract class State[+A] {
    def get: A

    def fulfill[B >: A](a: => B, promise: Promise[B]): Unit

    val fulfilled: Boolean
    val threw: Boolean

    def break(promise: Promise[_]): Unit
  }

  protected class Thrown(e: Throwable) extends State[Nothing] {
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

  protected class Fulfilled[A](val get: A) extends State[A] {
    def fulfill[B >: A](a: => B, promise: Promise[B]) {}

    val fulfilled = true
    val threw = false

    def break(promise: Promise[_]) {}
  }

  protected object Unfulfilled extends State[Nothing] {
    // the only way get gets here is if the promise is broken
    def get: Nothing = throw new BrokenException

    def fulfill[B](a: => B, promise: Promise[B]) {
      if (!promise.borked) {
        try {
          promise.v = new Fulfilled(a)
          promise.latch.countDown()
          val as = promise.waiting
          while (!as.isEmpty) as.remove()(a)
        } catch {
          case e: Throwable => {
            promise.v = new Thrown(e)
            promise.latch.countDown()
            promise.waiting.clear() // kill teh hordes
          }
        }
      }
    }

    val fulfilled = false
    val threw = false

    def break(promise: Promise[_]) {
      promise.latch.countDown() // free the hordes
    }
  }

  sealed class BrokenException extends Exception

  protected[concurrent] abstract sealed class Signal[A] {
    def eval: Unit
  }

  protected[concurrent] class Done[A](a: => A, promise: Promise[A]) extends Signal[A] {
    def eval {
      promise.v.fulfill(a, promise)
    }
  }

  protected[concurrent] class Cont[A](k: A => Unit, promise: Promise[A]) extends Signal[A] {
    def eval {
      if (promise.v.fulfilled) k(promise.v.get)
      else promise.waiting.offer(k)
    }
  }

  protected[concurrent] class Break[A](promise: Promise[_]) extends Signal[A] {
    def eval {
      promise.v.break(promise)
    }
  }

}

trait PromiseInstances {

  import Promise._

  implicit def promiseInstance(implicit s: Strategy) = new Traverse[Promise] with Monad[Promise] with Comonad[Promise] {
    def cojoin[A](a: Promise[A]): Promise[Promise[A]] = promise(a)
    def pure[A](a: => A): Promise[A] = promise(a)
    def copure[A](p: Promise[A]): A = p.get
    def traverseImpl[G[_] : Applicative, A, B](fa: Promise[A])(f: (A) => G[B]): G[Promise[B]] =
      Applicative[G].map(f(fa.get))(promise(_: B)(fa.strategy))
    def foldR[A, B](fa: Promise[A], z: B)(f: (A) => (=> B) => B): B = f(fa.get)(z)
    def bind[A, B](fa: Promise[A])(f: (A) => Promise[B]): Promise[B] = fa flatMap f
  }
}

trait PromiseFunctions {

  def promise[A](a: => A)(implicit s: Strategy): Promise[A] = {
    val p = new Promise[A] {
      implicit val strategy = s
    }
    p.e ! new Promise.Done(a, p)
    p
  }
}
