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
  private val e = actor[Signal[A]](_.eval)

  def get = {
    latch.await
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
    actual flatMap (a => this flatMap (g => if (equality.equal(a)(g)) speculation
    else {
      speculation.break
      promise(f(a))
    }))
  }
}

object Promise extends Promises {
  def apply[A](a: => A)(implicit s: Strategy): Promise[A] =
    promise(a)

  def promise[A](a: => A)(implicit s: Strategy): Promise[A] = {
    val p = new Promise[A] {
      implicit val strategy = s
    }
    p.e ! new Promise.Done(a, p)
    p
  }

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
          promise.latch.countDown
          val as = promise.waiting
          while (!as.isEmpty) as.remove()(a)
        } catch {
          case e: Throwable => {
            promise.v = new Thrown(e)
            promise.latch.countDown
            promise.waiting.clear // kill teh hordes
          }
        }
      }
    }

    val fulfilled = false
    val threw = false

    def break(promise: Promise[_]) {
      promise.latch.countDown // free the hordes
    }
  }

  sealed class BrokenException extends Exception

  protected abstract sealed class Signal[A] {
    def eval: Unit
  }

  protected class Done[A](a: => A, promise: Promise[A]) extends Signal[A] {
    def eval {
      promise.v.fulfill(a, promise)
    }
  }

  protected class Cont[A](k: A => Unit, promise: Promise[A]) extends Signal[A] {
    def eval {
      if (promise.v.fulfilled) k(promise.v.get)
      else promise.waiting.offer(k)
    }
  }

  protected class Break[A](promise: Promise[_]) extends Signal[A] {
    def eval {
      promise.v.break(promise)
    }
  }

}

trait Promises {

  import Promise._

  implicit def PromisePointed(implicit s: Strategy): Pointed[Promise] = new Pointed[Promise] {
    def point[A](a: => A) = promise(a)
  }

  implicit def PromiseFunctor(implicit s: Strategy): Functor[Promise] = new Functor[Promise] {
    def fmap[A, B](f: A => B) = _ map f
  }

  implicit def PromisePointedFunctor(implicit s: Strategy): PointedFunctor[Promise] =
    PointedFunctor.pointedFunctor[Promise]

  implicit def PromiseApplic(implicit s: Strategy): Applic[Promise] = new Applic[Promise] {
    def applic[A, B](f: Promise[A => B]) =
      a => f flatMap (a map _)
  }

  implicit def PromiseApplicFunctor(implicit s: Strategy): ApplicFunctor[Promise] =
    ApplicFunctor.applicFunctor


  implicit def PromiseApplicative(implicit s: Strategy): Applicative[Promise] =
    Applicative.applicative

  implicit def PromiseBind(implicit s: Strategy): Bind[Promise] = new Bind[Promise] {
    def bind[A, B](f: A => Promise[B]) =
      _ flatMap f
  }

  implicit def PromiseBindFunctor(implicit s: Strategy): BindFunctor[Promise] =
    BindFunctor.bindFunctor

  implicit def PromiseJoin(implicit s: Strategy): Join[Promise] = new Join[Promise] {
    def join[A] =
      _ flatMap identity
  }

  implicit def PromiseMonad(implicit s: Strategy): Monad[Promise] =
    Monad.monadBP

  implicit def PromiseCoPointed: CoPointed[Promise] = new CoPointed[Promise] {
    def coPoint[A] = a => a.get
  }

  implicit def PromiseCoPointedFunctor: CoPointedFunctor[Promise] =
    CoPointedFunctor.coPointedFunctor[Promise]

  implicit def PromiseCoJoin: CoJoin[Promise] = new CoJoin[Promise] {
    def coJoin[A] = a => promise(a)(a.strategy)
  }

  implicit def PromiseExtend: Extend[Promise] =
    Extend.extend[Promise]

  implicit def PromiseCoMonad: CoMonad[Promise] =
    CoMonad.coMonadJP

  implicit def PromiseTraverse: Traverse[Promise] = new Traverse[Promise] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      a => implicitly[Applicative[F]].fmap(promise(_: B)(a.strategy))(f(a.get))
  }

}
