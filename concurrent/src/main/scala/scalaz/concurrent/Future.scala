package scalaz.concurrent

import java.util.concurrent.{Callable, ConcurrentLinkedQueue, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import collection.JavaConversions._

import scalaz.Nondeterminism
import scalaz.Free.Trampoline
import scalaz.Trampoline
import scalaz.syntax.monad._

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
 * `scalaz.Nondeterminsm` interface. This simplifies our implementation 
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
trait Future[+A] {
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
   * Also see `runAsync`. 
   */
  def listen(cb: A => Trampoline[Unit]): Unit = 
    this.step match {
      case Now(a) => cb(a) 
      case Async(onFinish) => onFinish(cb)
      case BindAsync(onFinish, g) => 
        onFinish(x => Trampoline.delay(g(x)) map (_ listen cb))
    }

  /**
   * Run this computation to obtain an `A`, so long as `cancel` remains false. 
   * Because of trampolining, we get frequent opportunities to cancel
   * while stepping through the trampoline, so this should provide a fairly 
   * robust means of cancellation.  
   */
  def listenInterruptibly(cb: A => Trampoline[Unit], cancel: AtomicBoolean): Unit = 
    this.stepInterruptibly(cancel) match {
      case Now(a) if !cancel.get => cb(a) 
      case Async(onFinish) if !cancel.get => onFinish(cb)
      case BindAsync(onFinish, g) if !cancel.get => 
        onFinish(x => 
          if (!cancel.get) Trampoline.delay(g(x)) map (_ listenInterruptibly (cb, cancel))
          else Trampoline.done(()))
      case _ if cancel.get => ()
    }

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
   * waiting for the result. Note that this results  
   */
  def start: Future[A] = {
    val latch = new java.util.concurrent.CountDownLatch(1)
    @volatile var result: Option[A] = None
    runAsync { a => result = Some(a); latch.countDown } 
    delay { latch.await; result.get }
  }

  /** 
   * Run this `Future`, passing the result to the given callback once available. 
   * Any pure, non-asynchronous computation at the head of this `Future` will
   * be forced in the calling thread. At the first `Async` encountered, control
   * switches to whatever thread backs the `Async` and this function returns.
   */
  def runAsync(cb: A => Unit): Unit = 
    listen(a => Trampoline.done(cb(a)))

  /** 
   * Run this computation to obtain an `A`, so long as `cancel` remains false. 
   * Because of trampolining, we get frequent opportunities to cancel
   * while stepping through the trampoline, this should provide a fairly 
   * robust means of cancellation.  
   */
  def runAsyncInterruptibly(cb: A => Unit, cancel: AtomicBoolean): Unit = 
    listenInterruptibly(a => Trampoline.done(cb(a)), cancel)

  /** Run this `Future` and block awaiting its result. */
  def run: A = this match {
    case Now(a) => a
    case _ => {
      val latch = new java.util.concurrent.CountDownLatch(1) 
      @volatile var result: Option[A] = None
      runAsync { a => result = Some(a); latch.countDown }
      latch.await
      result.get
    }
  }
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

  implicit val futureInstance = new Nondeterminism[Future] {
    def bind[A,B](fa: Future[A])(f: A => Future[B]): Future[B] =   
      fa flatMap f
    def point[A](a: => A): Future[A] = now(a)

    def chooseAny[A](h: Future[A], t: Seq[Future[A]]): Future[(A, Seq[Future[A]])] = {
      // The details of this implementation are a bit tricky, but general
      // idea is to run all `fs` in parallel, with each decrementing
      // a central CountDownLatch; we then return a Future that awaits on 
      // this latch, then returns whichever result became available first
      // 
      // To account for the fact that the losing computations are still 
      // running, we construct special 'residual' Futures for the losers,
      // that will first return from the already running computation, 
      // then revert back to running the original Future

      Async { (cb: Tuple2[A,Seq[Future[A]]] => Trampoline[Unit]) => 
        @volatile var result: Option[(A, Int)] = None
        val latch = new CountDownLatch(1)
        val fs = h +: t
        // we keep a separate latch and atomic reference for purposes of 
        // computing a residual Future should each Future lose
        val fs2: IndexedSeq[(Future[A], CountDownLatch, AtomicReference[A])] = 
          fs.toIndexedSeq.map { 
            (f: Future[A]) => (f, new CountDownLatch(1), new AtomicReference[A])
          }
        fs2.zipWithIndex.foreach { case ((f,flatch,ref), ind) => f.runAsync { a =>
          ref.set(a)
          // actually ok if two threads clobber each other here
          if (!result.isDefined) result = Some((a, ind))
          flatch.countDown 
          latch.countDown
        }}
        latch.await // wait for any one of the threads to finish
        val Some((a, ind)) = result // extract the winner
        // for all the losing futures, we compute a 'residual', which includes the
        // 'rest' of the current, partially completed computation, followed by a 
        // repetition of the original computation
        val residuals = fs2.zipWithIndex collect { case ((f, latch, ref), i) if i != ind => 
          val used = new AtomicBoolean(false)
          Async { (cb: A => Trampoline[Unit]) => 
            if (used.get) f.listen(cb) 
            else {
              // A bit of trickiness here, since two threads may listen to this
              // Async simultaneously, and we only want one to receive the value
              // inside `ref`. To ensure this, we race to set the `used` flag.
              // Whichever one wins gets the value inside `ref`, the other just
              // delegates to `f`.
              latch.await
              if (used.compareAndSet(false, true)) 
                cb(ref.get).run
              else
                f.listen(cb)
            }
          }
        }
        cb((a, residuals)).run
      }
    }

    // Optimized implementation has all Futures dump to a shared queue, then
    // waits for all to finish
    override def gatherUnordered[A](fs: Seq[Future[A]]): Future[List[A]] =
      Async { cb => 
        val latch = new CountDownLatch(fs.length)
        val results = new ConcurrentLinkedQueue[A] 
        fs.foreach(_ runAsync { a => 
          results.add(a)
          latch.countDown
        })
        latch.await
        cb(results.toList).run
      }
  }

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

}
