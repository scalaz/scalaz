package scalaz
package concurrent

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

sealed trait Promise[A] {

  import Promise._
  import Actor._
  import Run._

  implicit val strategy: Strategy
  private val latch = new CountDownLatch(1)
  private val waiting = new ConcurrentLinkedQueue[Waiting[A]]
  @volatile private var state: Promise.State[A] = Promise.Unfulfilled
  @volatile private var borked: Boolean = false
  lazy private[concurrent] val e = actor[Signal[A]](_.eval, onError)

  def get = {
    latch.await()
    state.get
  }

  def to(k: A => Unit, err: Throwable => Unit = e.onError): Unit = e ! new Cont(k, err, this)

  private def onError(e: Throwable): Unit = {
    state = new Thrown(e)
    latch.countDown()
    while (!waiting.isEmpty) waiting.remove().err(e)
  }

  def fulfill(a: => A): Unit = e ! new Done(a, this)

  def fulfilled: Boolean = state.fulfilled

  def threw: Boolean = state.threw

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
    to(a => f(a).to(Run[B](b => r fulfill b), r.onError), r.onError)
    r
  }

  def filter(p: A => Boolean): Promise[A] = {
    val r = new Promise[A] {
      implicit val strategy = Promise.this.strategy
    }
    to(a => promise(p(a)).to(Run[Boolean](b => if (b) r fulfill a else r.break), r.onError), r.onError)
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

  override def toString = "<promise>"
}

object Promise extends PromiseFunctions with PromiseInstances {
  private case class Waiting[A](ok: A => Unit, err: Throwable => Unit)

  def apply[A](a: => A)(implicit s: Strategy): Promise[A] =
    promise(a)

  protected sealed abstract class State[+A] {
    def get: A

    def fulfill[B >: A](a: => B, promise: Promise[B]): Unit

    val fulfilled: Boolean
    val threw: Boolean

    def break(promise: Promise[_]): Unit
  }

  protected case class Thrown(e: Throwable) extends State[Nothing] {
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

  protected case class Fulfilled[A](val get: A) extends State[A] {
    def fulfill[B >: A](a: => B, promise: Promise[B]) {}

    val fulfilled = true
    val threw = false

    def break(promise: Promise[_]) {}
  }

  protected case object Unfulfilled extends State[Nothing] {
    // the only way get gets here is if the promise is broken
    def get: Nothing = throw new BrokenException

    def fulfill[B](a: => B, promise: Promise[B]) {
      if (!promise.borked) {
        try {
          promise.state = new Fulfilled(a)
          promise.latch.countDown()
          val as = promise.waiting
          while (!as.isEmpty) as.remove().ok(a)
        } catch {
          case e: Throwable => {
            promise.onError(e)
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
      promise.state.fulfill(a, promise)
    }
  }

  protected[concurrent] class Cont[A](k: A => Unit, err: Throwable => Unit, promise: Promise[A]) extends Signal[A] {
    def eval {
      promise.state match {
        case Fulfilled(a) => k(a)
        case Unfulfilled => promise.waiting.offer(Waiting(k, err))
        case Thrown(e) => err(e)
      }
    }
  }

  protected[concurrent] class Break[A](promise: Promise[_]) extends Signal[A] {
    def eval {
      promise.state.break(promise)
    }
  }

}

trait PromiseInstances {

  import Promise._

  implicit def promiseInstance(implicit s: Strategy) = new Traverse[Promise] with Monad[Promise] with Comonad[Promise] {
    override def cojoin[A](a: Promise[A]): Promise[Promise[A]] = promise(a)
    def cobind[A, B](fa: Promise[A])(f: (Promise[A]) => B): Promise[B] = promise(f(fa))
    def point[A](a: => A): Promise[A] = promise(a)
    def copoint[A](p: Promise[A]): A = p.get
    def traverseImpl[G[_] : Applicative, A, B](fa: Promise[A])(f: A => G[B]): G[Promise[B]] =
      Applicative[G].map(f(fa.get))(promise(_: B)(fa.strategy))
    override def foldRight[A, B](fa: Promise[A], z: => B)(f: (A, => B) => B): B = f(fa.get, z)
    def bind[A, B](fa: Promise[A])(f: A => Promise[B]): Promise[B] = fa flatMap f
  }
}

trait PromiseFunctions {
  def emptyPromise[A](implicit s: Strategy): Promise[A] =
    new Promise[A] {
      implicit val strategy = s
    }

  def promise[A](a: => A)(implicit s: Strategy): Promise[A] = {
    val p = emptyPromise[A]
    p.e ! new Promise.Done(a, p)
    p
  }
}
