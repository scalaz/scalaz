package scalaz.concurrent

import java.util.concurrent.{Callable, ConcurrentLinkedQueue, ExecutorService, TimeoutException, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean, AtomicReference}

import collection.JavaConversions._
import scalaz.Tags.Parallel

import scalaz._
import scalaz.Free.Trampoline
import scalaz.syntax.monad._

import scala.concurrent.SyncVar
import scala.concurrent.duration._

/**
 * `Future` is a trampolined computation producing an `A` that may
 * include asynchronous steps. Like `Trampoline`, arbitrary
 * monadic expressions involving `map` and `flatMap` are guaranteed
 * to use constant stack space. But in addition, one may construct a
 * `Future` from an asynchronous computation, represented as a
 * function, `listen: (A => Unit) => Unit`, which registers a callback
 * that will be invoked when the result becomes available. This makes
 * `Future` useful as a concurrency primitive and as a control
 * structure for wrapping callback-based APIs with a more
 * straightforward, monadic API.
 *
 * Unlike the `Future` implementation in scala 2.10, `map` and
 * `flatMap` do NOT spawn new tasks and do not require an implicit
 * `ExecutionContext`. Instead, `map` and `flatMap` merely add to
 * the current (trampolined) continuation that will be run by the
 * 'current' thread, unless explicitly forked via `Future.fork` or
 * `Future.apply`. This means that `Future` achieves much better thread
 * reuse than the 2.10 implementation and avoids needless thread
 * pool submit cycles.
 *
 * `Future` also differs from the scala 2.10 `Future` type in that it
 * does not necessarily represent a _running_ computation. Instead, we
 * reintroduce nondeterminism _explicitly_ using the functions of the
 * `scalaz.Nondeterminism` interface. This simplifies our implementation
 * and makes code easier to reason about, since the order of effects
 * and the points of nondeterminism are made fully explicit and do not
 * depend on Scala's evaluation order.
 *
 * IMPORTANT NOTE: `Future` does not include any error handling and
 * should generally only be used as a building block by library
 * writers who want to build on `Future`'s capabilities but wish to
 * design their own error handling strategy. See
 * `scalaz.concurrent.Task` for a type that extends `Future` with
 * proper error handling -- it is merely a wrapper for
 * `Future[Throwable \/ A]` with a number of additional
 * convenience functions.
 */
sealed abstract class Future[+A] {
  import Future._

  def flatMap[B](f: A => Future[B]): Future[B] = this match {
    case Now(a) => Suspend(() => f(a))
    case Suspend(thunk) => BindSuspend(thunk, f)
    case Async(listen) => BindAsync(listen, f)
    case BindSuspend(thunk, g) =>
      Suspend(() => BindSuspend(thunk, g andThen (_ flatMap f)))
    case BindAsync(listen, g) =>
      Suspend(() => BindAsync(listen, g andThen (_ flatMap f)))
  }

  def map[B](f: A => B): Future[B] =
    flatMap(f andThen (b => Future.now(b)))

  /**
   * Run this computation to obtain an `A`, then invoke the given callback.
   * Also see `unsafePerformAsync`.
   */
  def unsafePerformListen(cb: A => Trampoline[Unit]): Unit =
    (this.step: @unchecked) match {
      case Now(a) => cb(a).run
      case Async(onFinish) => onFinish(cb)
      case BindAsync(onFinish, g) =>
        onFinish(x => Trampoline.delay(g(x)) map (_ unsafePerformListen cb))
    }

  
  @deprecated("use unsafePerformListen", "7.2")
  def listen(cb: A => Trampoline[Unit]): Unit =
    unsafePerformListen(cb)
  
  /**
   * Run this computation to obtain an `A`, so long as `cancel` remains false.
   * Because of trampolining, we get frequent opportunities to cancel
   * while stepping through the trampoline, so this should provide a fairly
   * robust means of cancellation.
   */
  def unsafePerformListenInterruptibly(cb: A => Trampoline[Unit], cancel: AtomicBoolean): Unit =
    this.stepInterruptibly(cancel) match {
      case Now(a) if !cancel.get => cb(a).run
      case Async(onFinish) if !cancel.get =>
        onFinish(a =>
          if (!cancel.get) cb(a)
          else Trampoline.done(()))
      case BindAsync(onFinish, g) if !cancel.get =>
        onFinish(x =>
          if (!cancel.get) Trampoline.delay(g(x)) map (_ unsafePerformListenInterruptibly (cb, cancel))
          else Trampoline.done(()))
      case _ if cancel.get => ()
    }

  @deprecated("use unsafePerformListenInterruptibly", "7.2")
  def listenInterruptibly(cb: A => Trampoline[Unit], cancel: AtomicBoolean): Unit =
    unsafePerformListenInterruptibly(cb, cancel)
  
  /**
   * Evaluate this `Future` to a result, or another asynchronous computation.
   * This has the effect of stripping off any 'pure' trampolined computation at
   * the start of this `Future`.
   */
  @annotation.tailrec
  final def step: Future[A] = this match {
    case Suspend(thunk) => thunk().step
    case BindSuspend(thunk, f) => (thunk() flatMap f).step
    case _ => this
  }

  /** Like `step`, but may be interrupted by setting `cancel` to true. */
  @annotation.tailrec
  final def stepInterruptibly(cancel: AtomicBoolean): Future[A] =
    if (!cancel.get) this match {
      case Suspend(thunk) => thunk().stepInterruptibly(cancel)
      case BindSuspend(thunk, f) => (thunk() flatMap f).stepInterruptibly(cancel)
      case _ => this
    }
    else this

  /**
   * Begins running this `Future` and returns a new future that blocks
   * waiting for the result. Note that this will start executing side effects 
   * immediately, and is thus morally equivalent to `unsafePerformIO`. The 
   * resulting `Future` cannot be rerun to repeat the effects.
   * 
   * Use with care.
   */
  def unsafeStart: Future[A] = {
    val latch = new java.util.concurrent.CountDownLatch(1)
    @volatile var result: Option[A] = None
    unsafePerformAsync { a => result = Some(a); latch.countDown }
    delay { latch.await; result.get }
  }
  
  @deprecated("use unsafeStart", "7.2")
  def start: Future[A] =
    unsafeStart

  /**
   * Run this `Future`, passing the result to the given callback once available.
   * Any pure, non-asynchronous computation at the head of this `Future` will
   * be forced in the calling thread. At the first `Async` encountered, control
   * switches to whatever thread backs the `Async` and this function returns.
   */
  def unsafePerformAsync(cb: A => Unit): Unit =
    unsafePerformListen(a => Trampoline.done(cb(a)))

  @deprecated("use unsafePerformAsync", "7.2")
  def runAsync(cb: A => Unit): Unit =
    unsafePerformAsync(cb)
    
  /**
   * Run this computation to obtain an `A`, so long as `cancel` remains false.
   * Because of trampolining, we get frequent opportunities to cancel
   * while stepping through the trampoline, this should provide a fairly
   * robust means of cancellation.
   */
  def unsafePerformAsyncInterruptibly(cb: A => Unit, cancel: AtomicBoolean): Unit =
    unsafePerformListenInterruptibly(a => Trampoline.done(cb(a)), cancel)

  @deprecated("use unsafePerformAsyncInterruptibly", "7.2")
  def runAsyncInterruptibly(cb: A => Unit, cancel: AtomicBoolean): Unit =
    unsafePerformAsyncInterruptibly(cb, cancel)
    
  /** Run this `Future` and block awaiting its result. */
  def unsafePerformSync: A = this match {
    case Now(a) => a
    case _ => {
      val latch = new java.util.concurrent.CountDownLatch(1)
      @volatile var result: Option[A] = None
      unsafePerformAsync { a => result = Some(a); latch.countDown }
      latch.await
      result.get
    }
  }

  @deprecated("use unsafePerformSync", "7.2")
  def run: A =
    unsafePerformSync
  
  /**
   * Run this `Future` and block until its result is available, or until
   * `timeoutInMillis` milliseconds have elapsed, at which point a `TimeoutException`
   * will be thrown and the `Future` will attempt to be canceled.
   */
  def unsafePerformSyncFor(timeoutInMillis: Long): A = 
    unsafePerformSyncAttemptFor(timeoutInMillis) match {
      case -\/(e) => throw e
      case \/-(a) => a
    }

  def unsafePerformSyncFor(timeout: Duration): A = 
    unsafePerformSyncFor(timeout.toMillis)

  @deprecated("use unsafePerformSyncFor", "7.2")
  def runFor(timeoutInMillis: Long): A =
    unsafePerformSyncFor(timeoutInMillis)

  @deprecated("use unsafePerformSyncFor", "7.2")
  def runFor(timeout: Duration): A =
    unsafePerformSyncFor(timeout)

    
  /** Like `unsafePerformSyncFor`, but returns `TimeoutException` as left value. 
    * Will not report any other exceptions that may be raised during computation of `A`*/
  def unsafePerformSyncAttemptFor(timeoutInMillis: Long): Throwable \/ A = {
    val sync = new SyncVar[Throwable \/ A]
    val interrupt = new AtomicBoolean(false)
    unsafePerformAsyncInterruptibly(a => sync.put(\/-(a)), interrupt)
    sync.get(timeoutInMillis).getOrElse {
      interrupt.set(true)
      -\/(new TimeoutException(s"Timed out after $timeoutInMillis milliseconds"))
    }
  }

  def unsafePerformSyncAttemptFor(timeout: Duration): Throwable \/ A = 
    unsafePerformSyncAttemptFor(timeout.toMillis)

  @deprecated("use unsafePerformSyncAttemptFor", "7.2")
  def attemptRunFor(timeoutInMillis: Long): Throwable \/ A =
    unsafePerformSyncAttemptFor(timeoutInMillis)

  @deprecated("use unsafePerformSyncAttemptFor", "7.2")
  def attemptRunFor(timeout: Duration): Throwable \/ A =
    unsafePerformSyncAttemptFor(timeout)
  
  /**
   * Returns a `Future` which returns a `TimeoutException` after `timeoutInMillis`,
   * and attempts to cancel the running computation.
   * This implementation will not block the future's execution thread
   */
  def timed(timeoutInMillis: Long)(implicit scheduler:ScheduledExecutorService): Future[Throwable \/ A] =
    //instead of run this though chooseAny, it is run through simple primitive, 
    //as we are never interested in results of timeout callback, and this is more resource savvy
    async[Throwable \/ A] { cb =>
      val cancel = new AtomicBoolean(false)
      val done = new AtomicBoolean(false)
      scheduler.schedule(new Runnable {
        def run() { 
          if (done.compareAndSet(false,true)) {
            cancel.set(true)
            cb(-\/(new TimeoutException(s"Timed out after $timeoutInMillis milliseconds")))
          } 
        }
      }
      , timeoutInMillis, TimeUnit.MILLISECONDS)
      
      unsafePerformAsyncInterruptibly(a => if(done.compareAndSet(false,true)) cb(\/-(a)), cancel) 
    }

  def timed(timeout: Duration)(implicit scheduler:ScheduledExecutorService = Strategy.DefaultTimeoutScheduler): Future[Throwable \/ A] =
    timed(timeout.toMillis)

  @deprecated("use timed", "7.2")
  def unsafePerformTimed(timeout: Duration)(implicit scheduler:ScheduledExecutorService = Strategy.DefaultTimeoutScheduler): Future[Throwable \/ A] =
    timed(timeout.toMillis)

  @deprecated("use timed", "7.2")
  def unsafePerformTimed(timeoutInMillis: Long)(implicit scheduler:ScheduledExecutorService): Future[Throwable \/ A] =
    timed(timeoutInMillis)

  /**
   * Returns a `Future` that delays the execution of this `Future` by the duration `t`.
   */
  def after(t: Duration): Future[A] =
    after(t.toMillis)

  def after(t: Long): Future[A] =
    Timer.default.valueWait((), t).flatMap(_ => this)
}

object Future {
  case class Now[+A](a: A) extends Future[A]
  case class Async[+A](onFinish: (A => Trampoline[Unit]) => Unit) extends Future[A]
  case class Suspend[+A](thunk: () => Future[A]) extends Future[A]
  case class BindSuspend[A,B](thunk: () => Future[A], f: A => Future[B]) extends Future[B]
  case class BindAsync[A,B](onFinish: (A => Trampoline[Unit]) => Unit,
                            f: A => Future[B]) extends Future[B]

  // NB: considered implementing Traverse and Comonad, but these would have
  // to run the Future; leaving out for now

  implicit val futureInstance: Nondeterminism[Future] = new Nondeterminism[Future] {
    def bind[A,B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa flatMap f
    def point[A](a: => A): Future[A] = delay(a)

    def chooseAny[A](h: Future[A], t: Seq[Future[A]]): Future[(A, Seq[Future[A]])] = {
      Async { cb =>
        // The details of this implementation are a bit tricky, but the general
        // idea is to run all futures in parallel, returning whichever result
        // becomes available first.

        // To account for the fact that the losing computations are still
        // running, we construct special 'residual' Futures for the losers
        // that will first return from the already running computation,
        // then revert back to running the original Future.
        val won = new AtomicBoolean(false) // threads race to set this

        val fs = (h +: t).view.zipWithIndex.map { case (f, ind) =>
          val used = new AtomicBoolean(false)
          val ref = new AtomicReference[A]
          val listener = new AtomicReference[A => Trampoline[Unit]](null)
          val residual = Async { (cb: A => Trampoline[Unit]) =>
             if (used.compareAndSet(false, true)) { // get residual value from already running Future
               if (listener.compareAndSet(null, cb)) {} // we've successfully registered ourself with running task
               else cb(ref.get).run // the running task has completed, use its result
             }
             else // residual value used up, revert to original Future
               f.unsafePerformListen(cb)
          }
          (ind, f, residual, listener, ref)
        }.toIndexedSeq

        fs.foreach { case (ind, f, residual, listener, ref) =>
          f.unsafePerformListen { a =>
            ref.set(a)
            val notifyWinner =
              // If we're the first to finish, invoke `cb`, passing residuals
              if (won.compareAndSet(false, true))
                cb((a, fs.collect { case (i,_,rf,_,_) if i != ind => rf }))
              else {
                Trampoline.done(()) // noop; another thread will have already invoked `cb` w/ our residual
              }
            val notifyListener =
              if (listener.compareAndSet(null, finishedCallback))
                // noop; no listeners yet, any added after this will use result stored in `ref`
                Trampoline.done(())
              else // there is a registered listener, invoke it with the result
                listener.get.apply(a)
            notifyWinner *> notifyListener
          }
        }
      }
    }

    private[this] val finishedCallback: Any => Trampoline[Unit] =
      _ => sys.error("impossible, since there can only be one runner of chooseAny")

    // implementation runs all threads, dumping to a shared queue
    // last thread to finish invokes the callback with the results
    override def reduceUnordered[A, M](fs: Seq[Future[A]])(implicit R: Reducer[A, M]): Future[M] =
      fs match {
      case Seq() => Future.now(R.zero)
      case Seq(f) => f.map(R.unit)
      case other => Async { cb =>
        val results = new ConcurrentLinkedQueue[M]
        val c = new AtomicInteger(fs.size)

        fs.foreach { f =>
          f.unsafePerformListen { a =>
            // Try to reduce number of values in the queue
            val front = results.poll()
            if (front == null)
              results.add(R.unit(a))
            else
              results.add(R.cons(a, front))

            // only last completed f will hit the 0 here.
            if (c.decrementAndGet() == 0)
              cb(results.toList.foldLeft(R.zero)((a, b) => R.append(a, b)))
            else Trampoline.done(())
          }
        }
      }
    }
  }

  /** type for Futures which need to be executed in parallel when using an Applicative instance */
  type ParallelFuture[A] = Future[A] @@ Parallel

  /**
   * This Applicative instance runs Futures in parallel.
   *
   * It is different from the Applicative instance obtained from Monad[Future] which runs futures sequentially.
   */
  implicit val futureParallelApplicativeInstance: Applicative[ParallelFuture] =
    futureInstance.parallel

  /** Convert a strict value to a `Future`. */
  def now[A](a: A): Future[A] = Now(a)

  /**
   * Promote a non-strict value to a `Future`. Note that since `Future` is
   * unmemoized, this will recompute `a` each time it is sequenced into a
   * larger computation. Memoize `a` with a lazy value before calling this
   * function if memoization is desired.
   */
  def delay[A](a: => A): Future[A] = Suspend(() => Now(a))

  /**
   * Returns a `Future` that produces the same result as the given `Future`,
   * but forks its evaluation off into a separate (logical) thread, using
   * the given `ExecutorService`. Note that this forking is only described
   * by the returned `Future`--nothing occurs until the `Future` is run.
   */
  def fork[A](a: => Future[A])(implicit pool: ExecutorService = Strategy.DefaultExecutorService): Future[A] =
    Future(a).join

  /**
   * Produce `f` in the main trampolining loop, `Future.step`, using a fresh
   * call stack. The standard trampolining primitive, useful for avoiding
   * stack overflows.
   */
  def suspend[A](f: => Future[A]): Future[A] = Suspend(() => f)

  /**
   * Create a `Future` from an asynchronous computation, which takes the form
   * of a function with which we can register a callback. This can be used
   * to translate from a callback-based API to a straightforward monadic
   * version. See `Task.async` for a version that allows for asynchronous
   * exceptions.
   */
  def async[A](listen: (A => Unit) => Unit): Future[A] =
    Async((cb: A => Trampoline[Unit]) => listen { a => cb(a).run })

  /** Create a `Future` that will evaluate `a` using the given `ExecutorService`. */
  def apply[A](a: => A)(implicit pool: ExecutorService = Strategy.DefaultExecutorService): Future[A] = Async { cb =>
    pool.submit { new Callable[Unit] { def call = cb(a).run }}
  }

  /** Create a `Future` that will evaluate `a` after at least the given delay. */
  def schedule[A](a: => A, delay: Duration)(implicit pool: ScheduledExecutorService =
      Strategy.DefaultTimeoutScheduler): Future[A] =
    Async { cb =>
      pool.schedule(new Callable[Unit] {
        def call = cb(a).run
      }, delay.toMillis, TimeUnit.MILLISECONDS)
    }

  /** Calls `Nondeterminism[Future].gatherUnordered`.
   * @since 7.0.3
   */
  def gatherUnordered[A](fs: Seq[Future[A]]): Future[List[A]] =
    futureInstance.gatherUnordered(fs)

  def reduceUnordered[A, M](fs: Seq[Future[A]])(implicit R: Reducer[A, M]): Future[M] =
    futureInstance.reduceUnordered(fs)
}
