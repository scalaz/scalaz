package scalaz.concurrent

import java.util.concurrent.{ScheduledExecutorService, ConcurrentLinkedQueue, ExecutorService, Executors}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scalaz.{Catchable, Kleisli, Maybe, MonadError, Nondeterminism, Reducer, Traverse, \/, -\/, \/-, ~>}
import scalaz.syntax.monad._
import scalaz.std.list._
import scalaz.Free.Trampoline
import scalaz.Trampoline
import scalaz.\/._

import collection.JavaConversions._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future => StdFuture}

/*
 * `Task[A]` is a `scalaz.concurrent.Future[Throwable \/ A]`,
 * with some convenience functions for handling exceptions. Its
 * `Monad` and `Nondeterminism` instances are derived from `Future`.
 *
 * `Task` (and `Future`) differ in several key ways from the `Future`
 * implementation in Scala 2.10 , and have a number of advantages. See the
 * documentation for `scalaz.concurrent.Future` for more information.
 *
 * `Task` is exception-safe when constructed using the primitives
 * in the companion object, but when calling the constructor, you
 * are responsible for ensuring the exception safety of the provided
 * `Future`.
 */
class Task[+A](val get: Future[Throwable \/ A]) {

  def flatMap[B](f: A => Task[B]): Task[B] =
    new Task(get flatMap {
      case -\/(e) => Future.now(-\/(e))
      case \/-(a) => Task.Try(f(a)) match {
        case e @ -\/(_) => Future.now(e)
        case \/-(task) => task.get
      }
    })

  def map[B](f: A => B): Task[B] =
    new Task(get map { _ flatMap {a => Task.Try(f(a))} })

  /** 'Catches' exceptions in the given task and returns them as values. */
  def attempt: Task[Throwable \/ A] =
    new Task(get map {
      case -\/(e) => \/-(-\/(e))
      case \/-(a) => \/-(\/-(a))
    })

  /**
   * Returns a new `Task` in which `f` is scheduled to be run on completion.
   * This would typically be used to release any resources acquired by this
   * `Task`.
   */
  def onFinish(f: Option[Throwable] => Task[Unit]): Task[A] =
    new Task(get flatMap {
      case -\/(e) => f(Some(e)).get *> Future.now(-\/(e))
      case r => f(None).get *> Future.now(r)
    })

  /**
   * Calls `attempt` and handles some exceptions using the given partial
   * function, calling Task.now on the result. Any nonmatching exceptions
   * are reraised.
   */
  def handle[B>:A](f: PartialFunction[Throwable,B]): Task[B] =
    handleWith(f andThen Task.now)

  /**
   * Calls `attempt` and handles some exceptions using the given partial
   * function. Any nonmatching exceptions are reraised.
   */
  def handleWith[B>:A](f: PartialFunction[Throwable,Task[B]]): Task[B] =
    attempt flatMap {
      case -\/(e) => f.lift(e) getOrElse Task.fail(e)
      case \/-(a) => Task.now(a)
    }

  /**
   * Runs this `Task`, and if it fails with an exception, runs `t2`.
   * This is rather coarse-grained. Use `attempt`, `handle`, and
   * `flatMap` for more fine grained control of exception handling.
   */
  def or[B>:A](t2: Task[B]): Task[B] =
    new Task(this.get flatMap {
      case -\/(e) => t2.get
      case a => Future.now(a)
    })

  /**
   * Run this `Task` and block until its result is available. This will
   * throw any exceptions generated by the `Task`. To return exceptions
   * in an `\/`, use `attemptRun`.
   */
  def run: A = get.run match {
    case -\/(e) => throw e
    case \/-(a) => a
  }

  /** Like `run`, but returns exceptions as values. */
  def attemptRun: Throwable \/ A =
    try get.run catch { case t: Throwable => -\/(t) }

  /**
   * Run this computation to obtain an `A`, so long as `cancel` remains false.
   * Because of trampolining, we get frequent opportunities to cancel
   * while stepping through the trampoline, this should provide a fairly
   * robust means of cancellation.
   */
  def runAsyncInterruptibly(f: (Throwable \/ A) => Unit, cancel: AtomicBoolean): Unit =
    get.runAsyncInterruptibly(f, cancel)

  /**
   * Similar to `runAsyncInterruptibly(f,cancel)` except instead of interrupting by setting cancel to true,
   * It returns the function, that, when applied will interrupt the task.
   *
   * This allows "deterministic" completion of task computation
   * even if it was interrupted.
   * That means task will complete even when interrupted,
   * but with `TaskInterrupted` exception.
   *
   * Note 1: When Interrupted, the `f` callback will run in thread that called the `Interrupting` function () => Unit
   * Note 2: If task has handler like attempt, it won't get consulted for handling TaskInterrupted excpetion
   * @param f
   * @return
   */
  def runAsyncInterruptibly(f: (Throwable \/ A) => Unit) : () => Unit = {
    val completed : AtomicBoolean = new AtomicBoolean(false)
    val a = Actor[Option[Throwable \/ A]] ({
      case Some(r) if ! completed.get =>
        completed.set(true)
        f(r)
      case None if ! completed.get  =>
        completed.set(true)
        f(left(Task.TaskInterrupted))
      case _ => () //already completed
    })(Strategy.Sequential)

    get.runAsyncInterruptibly(r => a ! Some(r), completed)
    () => { a ! None }
  }

  /**
   * Run this computation to obtain either a result or an exception, then
   * invoke the given callback. Any pure, non-asynchronous computation at the
   * head of this `Future` will be forced in the calling thread. At the first
   * `Async` encountered, control to whatever thread backs the `Async` and
   * this function returns immediately.
   */
  def runAsync(f: (Throwable \/ A) => Unit): Unit =
    get.runAsync(f)

  /**
   * Run this `Task` and block until its result is available, or until
   * `timeoutInMillis` milliseconds have elapsed, at which point a `TimeoutException`
   * will be thrown and the `Future` will attempt to be canceled.
   */
  def runFor(timeoutInMillis: Long): A = get.runFor(timeoutInMillis) match {
    case -\/(e) => throw e
    case \/-(a) => a
  }

  def runFor(timeout: Duration): A = runFor(timeout.toMillis)

  /**
   * Like `runFor`, but returns exceptions as values. Both `TimeoutException`
   * and other exceptions will be folded into the same `Throwable`.
   */
  def attemptRunFor(timeoutInMillis: Long): Throwable \/ A =
    get.attemptRunFor(timeoutInMillis).join

  def attemptRunFor(timeout: Duration): Throwable \/ A = attemptRunFor(timeout.toMillis)

  /**
   * A `Task` which returns a `TimeoutException` after `timeoutInMillis`,
   * and attempts to cancel the running computation.
   */
  def timed(timeoutInMillis: Long)(implicit scheduler:ScheduledExecutorService): Task[A] =
    new Task(get.timed(timeoutInMillis).map(_.join))

  def timed(timeout: Duration)(implicit scheduler:ScheduledExecutorService =
  Strategy.DefaultTimeoutScheduler): Task[A] = timed(timeout.toMillis)

  /**
   * Retries this task if it fails, once for each element in `delays`,
   * each retry delayed by the corresponding duration, accumulating
   * errors into a list.
   * A retriable failure is one for which the predicate `p` returns `true`.
   */
  def retryAccumulating(delays: Seq[Duration], p: (Throwable => Boolean) = _.isInstanceOf[Exception]): Task[(A, List[Throwable])] =
    retryInternal(delays, p, true)

  /**
   * Retries this task if it fails, once for each element in `delays`,
   * each retry delayed by the corresponding duration.
   * A retriable failure is one for which the predicate `p` returns `true`.
   */
  def retry(delays: Seq[Duration], p: (Throwable => Boolean) = _.isInstanceOf[Exception]): Task[A] =
    retryInternal(delays, p, false).map(_._1)

  private def retryInternal(delays: Seq[Duration],
                            p: (Throwable => Boolean),
                            accumulateErrors: Boolean): Task[(A, List[Throwable])] = {
      def help(ds: Seq[Duration], es: => Stream[Throwable]): Future[Throwable \/ (A, List[Throwable])] = {
        def acc = if (accumulateErrors) es.toList else Nil
          ds match {
            case Seq() => get map (_. map(_ -> acc))
            case Seq(t, ts @_*) => get flatMap {
              case -\/(e) if p(e) =>
                help(ts, e #:: es) after t
              case x => Future.now(x.map(_ -> acc))
            }
        }
      }
      Task.async { help(delays, Stream()).runAsync }
    }

  /** Ensures that the result of this Task satisfies the given predicate, or fails with the given value. */
  def ensure(failure: => Throwable)(f: A => Boolean): Task[A] =
    flatMap(a => if(f(a)) Task.now(a) else Task.fail(failure))

  /**
   * Delays the execution of this `Task` by the duration `t`.
   */
  def after(t: Duration): Task[A] =
    new Task(get after t)
}

object Task {

  implicit val taskInstance: Nondeterminism[Task] with Catchable[Task] with MonadError[({type λ[α,β] = Task[β]})#λ,Throwable] = new Nondeterminism[Task] with Catchable[Task] with MonadError[({type λ[α,β] = Task[β]})#λ,Throwable] {
    val F = Nondeterminism[Future]
    def point[A](a: => A) = new Task(Future.delay(Try(a)))
    def bind[A,B](a: Task[A])(f: A => Task[B]): Task[B] =
      a flatMap f
    def chooseAny[A](h: Task[A], t: Seq[Task[A]]): Task[(A, Seq[Task[A]])] =
      new Task ( F.map(F.chooseAny(h.get, t map (_ get))) { case (a, residuals) =>
        a.map((_, residuals.map(new Task(_))))
      })
    override def gatherUnordered[A](fs: Seq[Task[A]]): Task[List[A]] = {
      new Task (F.map(F.gatherUnordered(fs.map(_ get)))(eithers =>
        Traverse[List].sequenceU(eithers)
      ))
    }
    def fail[A](e: Throwable): Task[A] = new Task(Future.now(-\/(e)))
    def attempt[A](a: Task[A]): Task[Throwable \/ A] = a.attempt
    def raiseError[A](e: Throwable): Task[A] = fail(e)
    def handleError[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] =
      fa.handleWith { case t => f(t) }
  }

  /** signals task was interrupted **/
  case object TaskInterrupted extends InterruptedException {
    override def fillInStackTrace = this
  }

  /** A `Task` which fails with the given `Throwable`. */
  def fail(e: Throwable): Task[Nothing] = new Task(Future.now(-\/(e)))

  /** Convert a strict value to a `Task`. Also see `delay`. */
  def now[A](a: A): Task[A] = new Task(Future.now(\/-(a)))

  /**
   * Promote a non-strict value to a `Task`, catching exceptions in
   * the process. Note that since `Task` is unmemoized, this will
   * recompute `a` each time it is sequenced into a larger computation.
   * Memoize `a` with a lazy value before calling this function if
   * memoization is desired.
   */
  def delay[A](a: => A): Task[A] = suspend(now(a))

  /**
   * Produce `f` in the main trampolining loop, `Future.step`, using a fresh
   * call stack. The standard trampolining primitive, useful for avoiding
   * stack overflows.
   */
  def suspend[A](a: => Task[A]): Task[A] = new Task(Future.suspend(
    Try(a.get) match {
      case -\/(e) => Future.now(-\/(e))
      case \/-(f) => f
  }))

  /** Create a `Future` that will evaluate `a` using the given `ExecutorService`. */
  def apply[A](a: => A)(implicit pool: ExecutorService = Strategy.DefaultExecutorService): Task[A] =
    new Task(Future(Try(a))(pool))

  /**
   * Create a `Future` that starts evaluating `a` using the given `ExecutorService` right away.
   * This will start executing side effects immediately, and is thus morally equivalent to
   * `unsafePerformIO`. The resulting `Task` cannot be rerun to repeat the effects.
   * Use with care.
   */
  def unsafeStart[A](a: => A)(implicit pool: ExecutorService = Strategy.DefaultExecutorService): Task[A] =
    new Task(Future(Task.Try(a))(pool).start)

  /**
   * Returns a `Future` that produces the same result as the given `Future`,
   * but forks its evaluation off into a separate (logical) thread, using
   * the given `ExecutorService`. Note that this forking is only described
   * by the returned `Future`--nothing occurs until the `Future` is run.
   */
  def fork[A](a: => Task[A])(implicit pool: ExecutorService = Strategy.DefaultExecutorService): Task[A] =
    apply(a).join


  /**
   * Create a `Future` from an asynchronous computation, which takes the form
   * of a function with which we can register a callback. This can be used
   * to translate from a callback-based API to a straightforward monadic
   * version. See `Task.async` for a version that allows for asynchronous
   * exceptions.
   */
  def async[A](register: ((Throwable \/ A) => Unit) => Unit): Task[A] =
    new Task(Future.async(register))

  /**
   * Like `Nondeterminism[Task].gatherUnordered`, but if `exceptionCancels` is true,
   * exceptions in any task try to immediately cancel all other running tasks. If
   * `exceptionCancels` is false, in the event of an error, all tasks are run to completion
   * before the error is returned.
   * @since 7.0.3
   */
  def gatherUnordered[A](tasks: Seq[Task[A]], exceptionCancels: Boolean = false): Task[List[A]] =
    reduceUnordered[A, List[A]](tasks, exceptionCancels)

  def reduceUnordered[A, M](tasks: Seq[Task[A]], exceptionCancels: Boolean = false)(implicit R: Reducer[A, M]): Task[M] =
    if (!exceptionCancels) taskInstance.reduceUnordered(tasks)
    else tasks match {
      // Unfortunately we cannot reuse the future's combinator
      // due to early terminating requirement on task
      // when task fails.  This also makes implementation a bit trickier
      case Seq() => Task.now(R.zero)
      case Seq(t) => t.map(R.unit)
      case _ => new Task(Future.Async { cb =>
        val interrupt = new AtomicBoolean(false)
        val results = new ConcurrentLinkedQueue[M]
        val togo = new AtomicInteger(tasks.size)

        tasks.foreach { t =>
          val handle: (Throwable \/ A) => Trampoline[Unit] = {
            case \/-(success) =>
              // Try to reduce number of values in the queue
              val front = results.poll()
              if (front == null)
                results.add(R.unit(success))
              else
                results.add(R.cons(success, front))

              // only last completed f will hit the 0 here.
              if (togo.decrementAndGet() == 0)
                cb(\/-(results.toList.foldLeft(R.zero)((a, b) => R.append(a, b))))
              else
                Trampoline.done(())
            case e@(-\/(failure)) =>
              // Only allow the first failure to invoke the callback, so we
              // race to set `togo` to 0 here.
              // If we win, invoke the callback with our error, otherwise, noop
              @annotation.tailrec
              def firstFailure: Boolean = {
                val current = togo.get
                if (current > 0) {
                  if (togo.compareAndSet(current,0)) true
                  else firstFailure
                }
                else false
              }

              if (firstFailure) // invoke `cb`, then cancel any computation not running yet
                // food for thought - might be safe to set the interrupt first
                // but, this may also kill `cb(e)`
                // could have separate AtomicBooleans for each task
                cb(e) *> Trampoline.delay { interrupt.set(true); () }
              else
                Trampoline.done(())
          }
          t.get.listenInterruptibly(handle, interrupt)
        }
      })
    }

  /** Utility function - evaluate `a` and catch and return any exceptions. */
  def Try[A](a: => A): Throwable \/ A =
    try \/-(a) catch { case e: Throwable => -\/(e) }

  def fromMaybe[A](ma: Maybe[A])(t: => Throwable): Task[A] =
    ma.cata(Task.now, Task.fail(t))

  def fromDisjunction[A <: Throwable, B](x: A \/ B): Task[B] =
    x.fold(Task.fail, Task.now)

  val stdFutureToTask: StdFuture ~> ({type λ[α] = Kleisli[Task, ExecutionContext, α]})#λ =
    new (StdFuture ~> ({type λ[α] = Kleisli[Task, ExecutionContext, α]})#λ) {
      def apply[A](fa: StdFuture[A]) = Kleisli(ec =>
        Task.async(register =>
          fa.onComplete(a =>
            register(scalaz.std.`try`.tryDisjunctionIso.to(a))
          )(ec)))
    }

  /** Warning: this runs the Task in order to convert it to a scala.concurrent.Future */
  val taskToStdFuture: Task ~> StdFuture =
    new (Task ~> StdFuture) {
      def apply[A](fa: Task[A]) = {
        val p = scala.concurrent.Promise[A]()
        fa.runAsync {
          case -\/(t) => p.failure(t)
          case \/-(a) => p.success(a)
        }
        p.future
      }
    }
}

