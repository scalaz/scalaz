// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scala.annotation.switch
import scala.concurrent.duration._

import scalaz.data.Disjunction
import scalaz.data.Disjunction._
import scalaz.data.Maybe
import scalaz.Void
import scalaz.<~<

import scalaz.effect.Errors._

/**
 * An `IO[E, A]` ("Eye-Oh of Eeh Aye") is an immutable data structure that
 * describes an effectful action that may fail with an `E`, run forever, or
 * produce a single `A` at some point in the future.
 *
 * Conceptually, this structure is equivalent to `EitherT[F, E, A]` for some
 * infallible effect monad `F`, but because monad transformers perform poorly
 * in Scala, this structure bakes in the `EitherT` without runtime overhead.
 *
 * `IO` values are ordinary immutable values, and may be used like any other
 * values in purely functional code. Because `IO` values just *describe*
 * effects, which must be interpreted by a separate runtime system, they are
 * entirely pure and do not violate referential transparency.
 *
 * `IO` values can efficiently describe the following classes of effects:
 *
 *  * **Pure Values** &mdash; `IO.point`
 *  * **Synchronous Effects** &mdash; `IO.sync`
 *  * **Asynchronous Effects** &mdash; `IO.async`
 *  * **Concurrent Effects** &mdash; `io.fork`
 *  * **Resource Effects** &mdash; `io.bracket`
 *
 * The concurrency model is based on *fibers*, a user-land lightweight thread,
 * which permit cooperative multitasking, fine-grained interruption, and very
 * high performance with large numbers of concurrently executing fibers.
 *
 * `IO` values compose with other `IO` values in a variety of ways to build
 * complex, rich, interactive applications. See the methods on `IO` for more
 * details about how to compose `IO` values.
 *
 * In order to integrate with Scala, `IO` values must be interpreted into the
 * Scala runtime. This process of interpretation executes the effects described
 * by a given immutable `IO` value. For more information on interpreting `IO`
 * values, see the default interpreter in `RTS` or the safe main function in
 * `SafeApp`.
 */
sealed abstract class IO[E, A] { self =>

  /**
   * Maps an `IO[E, A]` into an `IO[E, B]` by applying the specified `A => B` function
   * to the output of this action. Repeated applications of `map`
   * (`io.map(f1).map(f2)...map(f10000)`) are guaranteed stack safe to a depth
   * of at least 10,000.
   */
  final def map[B](f: A => B): IO[E, B] = (self.tag: @switch) match {
    case IO.Tags.Point =>
      val io = self.asInstanceOf[IO.Point[E, A]]

      new IO.Point(() => f(io.value()))

    case IO.Tags.Strict =>
      val io = self.asInstanceOf[IO.Strict[E, A]]

      new IO.Strict(f(io.value))

    case IO.Tags.Fail => self.asInstanceOf[IO[E, B]]

    case _ => new IO.FlatMap(self, (a: A) => new IO.Strict(f(a)))
  }

  /**
   * Creates a composite action that represents this action followed by another
   * one that may depend on the value produced by this one.
   *
   * {{{
   * val parsed = readFile("foo.txt").flatMap(file => parseFile(file))
   * }}}
   */
  final def flatMap[B](f0: A => IO[E, B]): IO[E, B] = new IO.FlatMap(self, f0)

  /**
   * Forks this action into its own separate fiber, returning immediately
   * without the value produced by this action.
   *
   * The `Fiber[E, A]` returned by this action can be used to interrupt the
   * forked fiber with some exception, or to join the fiber to "await" its
   * computed value.
   *
   * {{{
   * for {
   *   fiber <- subtask.fork
   *   // Do stuff...
   *   a <- subtask.join
   * } yield a
   * }}}
   */
  final def fork[E2]: IO[E2, Fiber[E, A]] = new IO.Fork(this, Maybe.empty)

  /**
   * A more powerful version of `fork` that allows specifying a handler to be
   * invoked on any exceptions that are not handled by the forked fiber.
   */
  final def fork0[E2](handler: Throwable => IO[Void, Unit]): IO[E2, Fiber[E, A]] =
    new IO.Fork(this, Maybe.just(handler))

  /**
   * Executes both this action and the specified action in parallel,
   * returning a tuple of their results. If either individual action fails,
   * then the returned action will fail.
   *
   * TODO: Replace with optimized primitive.
   */
  final def par[B](that: IO[E, B]): IO[E, (A, B)] =
    self
      .attempt[E]
      .raceWith(that.attempt[E])(
        {
          case (-\/(e), fiberb) => fiberb.interrupt(TerminatedException(e)) *> IO.fail(e)
          case (\/-(a), fiberb) => IO.absolve(fiberb.join).map((b: B) => (a, b))
        }, {
          case (-\/(e), fibera) => fibera.interrupt(TerminatedException(e)) *> IO.fail(e)
          case (\/-(b), fibera) => IO.absolve(fibera.join).map((a: A) => (a, b))
        }
      )

  /**
   * Races this action with the specified action, returning the first
   * result to produce an `A`, whichever it is. If neither action succeeds,
   * then the action will be terminated with some error.
   */
  final def race(that: IO[E, A]): IO[E, A] =
    raceWith(that)((a, fiber) => fiber.interrupt(LostRace(\/-(fiber))).const(a),
                   (a, fiber) => fiber.interrupt(LostRace(-\/(fiber))).const(a))

  /**
   * Races this action with the specified action, invoking the
   * specified finisher as soon as one value or the other has been computed.
   */
  final def raceWith[B, C](that: IO[E, B])(finishLeft: (A, Fiber[E, B]) => IO[E, C],
                                           finishRight: (B, Fiber[E, A]) => IO[E, C]): IO[E, C] =
    new IO.Race[E, A, B, C](self, that, finishLeft, finishRight)

  /**
   * Executes this action and returns its value, if it succeeds, but
   * otherwise executes the specified action.
   */
  final def orElse(that: => IO[E, A]): IO[E, A] =
    self.attempt.flatMap(_.fold(_ => that)(IO.now))

  /**
   * Maps over the error type. This can be used to lift a "smaller" error into
   * a "larger" error.
   */
  final def leftMap[E2](f: E => E2): IO[E2, A] =
    attempt[E2].flatMap {
      case -\/(e) => IO.fail[E2, A](f(e))
      case \/-(a) => IO.now[E2, A](a)
    }

  /**
   * Widens the error type to any supertype. While `leftMap` suffices for this
   * purpose, this method is significantly faster for this purpose.
   */
  final def widen[E2](implicit ev: E <~< E2): IO[E2, A] = {
    val _ = ev
    self.asInstanceOf[IO[E2, A]]
  }

  /**
   * Executes this action, capturing both failure and success and returning
   * the result in a `Disjunction`. This method is useful for recovering from
   * `IO` actions that may fail.
   *
   * The error parameter of the returned `IO` may be chosen arbitrarily, since
   * it is guaranteed the `IO` action does not raise any errors.
   */
  final def attempt[E2]: IO[E2, E \/ A] = (self.tag: @switch) match {
    case IO.Tags.Point =>
      val io = self.asInstanceOf[IO.Point[E, A]]

      new IO.Point(() => \/-(io.value()))

    case IO.Tags.Strict =>
      val io = self.asInstanceOf[IO.Strict[E, A]]

      new IO.Strict(\/-(io.value))

    case IO.Tags.SyncEffect =>
      val io = self.asInstanceOf[IO.SyncEffect[E, A]]

      new IO.SyncEffect(() => \/-(io.effect()))

    case IO.Tags.Fail =>
      val io = self.asInstanceOf[IO.Fail[E, A]]

      new IO.Strict(-\/(io.error))

    case _ => new IO.Attempt(self)
  }

  /**
   * When this action represents acquisition of a resource (for example,
   * opening a file, launching a thread, etc.), `bracket` can be used to ensure
   * the acquisition is not interrupted and the resource is released.
   *
   * The function does two things:
   *
   * 1. Ensures this action, which acquires the resource, will not be
   * interrupted. Of course, acquisition may fail for internal reasons (an
   * uncaught exception).
   * 2. Ensures the `release` action will not be interrupted, and will be
   * executed so long as this action successfully acquires the resource.
   *
   * In between acquisition and release of the resource, the `use` action is
   * executed.
   *
   * If the `release` action fails, then the entire action will fail even
   * if the `use` action succeeds. If this fail-fast behavior is not desired,
   * errors produced by the `release` action can be caught and ignored.
   *
   * {{{
   * openFile("data.json").bracket(closeFile) { file =>
   *   for {
   *     header <- readHeader(file)
   *     ...
   *   } yield result
   * }
   * }}}
   */
  final def bracket[B](release: A => IO[Void, Unit])(use: A => IO[E, B]): IO[E, B] =
    new IO.Bracket(this, (_: ExitResult[E, B], a: A) => release(a), use)

  /**
   * A more powerful version of `bracket` that provides information on whether
   * or not `use` succeeded to the release action.
   */
  final def bracket0[B](release: (ExitResult[E, B], A) => IO[Void, Unit])(use: A => IO[E, B]): IO[E, B] =
    new IO.Bracket(this, release, use)

  /**
   * A less powerful variant of `bracket` where the value produced by this
   * action is not needed.
   */
  final def bracket_[B](release: IO[Void, Unit])(use: IO[E, B]): IO[E, B] =
    self.bracket(_ => release)(_ => use)

  /**
   * Executes the specified finalizer, whether this action succeeds, fails, or
   * is interrupted.
   */
  final def ensuring(finalizer: IO[Void, Unit]): IO[E, A] =
    IO.unit.bracket(_ => finalizer)(_ => self)

  /**
   * Executes the release action only if there was an error.
   */
  final def bracketOnError[B](release: A => IO[Void, Unit])(use: A => IO[E, B]): IO[E, B] =
    bracket0(
      (r: ExitResult[E, B], a: A) =>
        r match {
          case ExitResult.Failed(_)     => release(a)
          case ExitResult.Terminated(_) => release(a)
          case _                        => IO.unit
      }
    )(use)

  /**
   * Runs the specified cleanup action if this action errors, providing the
   * error to the cleanup action. The cleanup action will not be interrupted.
   */
  final def onError(cleanup: Throwable \/ E => IO[Void, Unit]): IO[E, A] =
    IO.unit[E]
      .bracket0(
        (r: ExitResult[E, A], a: Unit) =>
          r match {
            case ExitResult.Failed(e)     => cleanup(Disjunction.right(e))
            case ExitResult.Terminated(e) => cleanup(Disjunction.left(e))
            case _                        => IO.unit
        }
      )(_ => self)

  /**
   * Supervises this action, which ensures that any fibers that are forked by
   * the action are interrupted with the specified error when this action
   * completes.
   */
  final def supervised(error: Throwable): IO[E, A] = new IO.Supervise(self, error)

  /**
   * Performs this action non-interruptibly. This will prevent the action from
   * being terminated externally, but the action may fail for internal reasons
   * (e.g. an uncaught error) or terminate due to defect.
   */
  final def uninterruptibly: IO[E, A] = new IO.Uninterruptible(self)

  /**
   * Recovers from all errors.
   *
   * {{{
   * openFile("config.json").catchAll(_ => IO.now(defaultConfig))
   * }}}
   */
  final def catchAll[E2](h: E => IO[E2, A]): IO[E2, A] =
    self.attempt[E2].flatMap {
      case -\/(e) => h(e)
      case \/-(a) => IO.now[E2, A](a)
    }

  /**
   * Recovers from some or all of the error cases.
   *
   * {{{
   * openFile("data.json").catchSome {
   *   case FileNotFoundException(_) => openFile("backup.json")
   * }
   * }}}
   */
  final def catchSome(pf: PartialFunction[E, IO[E, A]]): IO[E, A] = {
    def tryRescue(t: E): IO[E, A] =
      if (pf.isDefinedAt(t)) pf(t) else IO.fail(t)

    self.attempt[E].flatMap(_.fold(tryRescue)(IO.now))
  }

  /**
   * Maps this action to the specified constant while preserving the
   * effects of this action.
   */
  final def const[B](b: => B): IO[E, B] = self.map(_ => b)

  /**
   * A variant of `flatMap` that ignores the value produced by this action.
   */
  final def *>[B](io: => IO[E, B]): IO[E, B] = self.flatMap(_ => io)

  /**
   * Sequences the specified action after this action, but ignores the
   * value produced by the action.
   */
  final def <*[B](io: => IO[E, B]): IO[E, A] = self.flatMap(io.const(_))

  /**
   * Sequentially zips this effect with the specified effect using the
   * specified combiner function.
   */
  final def zipWith[B, C](that: IO[E, B])(f: (A, B) => C): IO[E, C] =
    self.flatMap(a => that.map(b => f(a, b)))

  /**
   * Repeats this action forever (until the first error).
   */
  final def forever[B]: IO[E, B] = self *> self.forever

  /**
   * Retries continuously until this action succeeds.
   */
  final def retry: IO[E, A] = self orElse retry

  /**
   * Retries this action the specified number of times, until the first success.
   * Note that the action will always be run at least once, even if `n < 1`.
   */
  final def retryN(n: Int): IO[E, A] = retryBackoff(n, 1.0, Duration.fromNanos(0))

  /**
   * Retries continuously until the action succeeds or the specified duration
   * elapses.
   */
  final def retryFor(duration: Duration): IO[E, Maybe[A]] =
    retry.map(Maybe.just[A]) race
      (IO.sleep[E](duration) *> IO.now[E, Maybe[A]](Maybe.empty[A]))

  /**
   * Retries continuously, increasing the duration between retries each time by
   * the specified multiplication factor, and stopping after the specified upper
   * limit on retries.
   */
  final def retryBackoff(n: Int, factor: Double, duration: Duration): IO[E, A] =
    if (n <= 1) self
    else self orElse (IO.sleep(duration) *> retryBackoff(n - 1, factor, duration * factor))

  /**
   * Repeats this action continuously until the first error, with the specified
   * interval between each full execution.
   */
  final def repeat[B](interval: Duration): IO[E, B] =
    self *> IO.sleep(interval) *> repeat(interval)

  /**
    * Repeats this action n time.
    */
  final def repeatN(n: Int): IO[E, A] = if (n <= 1) self else self *> repeatN(n - 1)

  /**
    * Repeats this action n time with a specified function, which computes
    * a return value.
    */
  final def repeatNFold[B](n: Int)(b: B, f: (B, A) => B): IO[E, B] =
    if (n <= 1) IO.now(b) else self.flatMap(a => repeatNFold(n - 1)(f(b, a), f))

  /**
   * Repeats this action continuously until the first error, with the specified
   * interval between the start of each full execution. Note that if the
   * execution of this action takes longer than the specified interval, then the
   * action will instead execute as quickly as possible, but not
   * necessarily at the specified interval.
   */
  final def repeatFixed[B](interval: Duration): IO[E, B] =
    repeatFixed0(IO.sync(System.nanoTime()))(interval)

  final def repeatFixed0[B](nanoTime: IO[Void, Long])(interval: Duration): IO[E, B] =
    IO.flatten(nanoTime[E].flatMap { start =>
      val gapNs = interval.toNanos

      def tick[B](n: Int): IO[E, B] =
        self *> nanoTime[E].flatMap { now =>
          val await = ((start + n * gapNs) - now).max(0L)

          IO.sleep(await.nanoseconds) *> tick[B](n + 1)
        }

      tick(1)
    })


  /**
    * Repeats this action continuously until the function returns false.
    */
  final def doWhile(f: A => Boolean): IO[E, A] =
    self.flatMap(a => if (f(a)) doWhile(f) else IO.now(a))

  /**
   * Maps this action to one producing unit, but preserving the effects of
   * this action.
   */
  final def toUnit: IO[E, Unit] = const(())

  /**
   * Calls the provided function with the result of this action, and
   * sequences the resulting action after this action, but ignores the
   * value produced by the action.
   *
   * {{{
   * readFile("data.json").peek(putStrLn)
   * }}}
   */
  final def peek[B](f: A => IO[E, B]): IO[E, A] = self.flatMap(a => f(a).const(a))

  /**
   * Times out this action by the specified duration.
   *
   * {{{
   * action.timeout(1.second)
   * }}}
   */
  final def timeout(duration: Duration): IO[E, Maybe[A]] = {
    val timer = IO.now[E, Maybe[A]](Maybe.empty[A])

    self.map(Maybe.just[A]).race(timer.delay(duration))
  }

  /**
   * Returns a new action that executes this one and times the execution.
   */
  final def timed: IO[E, (Duration, A)] = timed0(IO.sync(System.nanoTime()))

  /**
   * A more powerful variation of `timed` that allows specifying the clock.
   */
  final def timed0(nanoTime: IO[E, Long]): IO[E, (Duration, A)] =
    summarized[Long, Duration]((start, end) => Duration.fromNanos(end - start))(nanoTime)

  /**
   * Summarizes a action by computing some value before and after execution, and
   * then combining the values to produce a summary, together with the result of
   * execution.
   */
  final def summarized[B, C](f: (B, B) => C)(summary: IO[E, B]): IO[E, (C, A)] =
    for {
      start <- summary
      value <- self
      end   <- summary
    } yield (f(start, end), value)

  /**
   * Delays this action by the specified amount of time.
   */
  final def delay(duration: Duration): IO[E, A] =
    IO.sleep(duration) *> self

  /**
   * Runs this action in a new fiber, resuming when the fiber terminates.
   */
  final def run[E2]: IO[E2, ExitResult[E, A]] = new IO.Run(self)

  /**
   * An integer that identifies the term in the `IO` sum type to which this
   * instance belongs (e.g. `IO.Tags.Point`).
   */
  def tag: Int
}

object IO extends IOInstances {
  final object Tags {
    final val FlatMap         = 0
    final val Point           = 1
    final val Strict          = 2
    final val SyncEffect      = 3
    final val Fail            = 4
    final val AsyncEffect     = 5
    final val AsyncIOEffect   = 6
    final val Attempt         = 7
    final val Fork            = 8
    final val Race            = 9
    final val Suspend         = 10
    final val Bracket         = 11
    final val Uninterruptible = 12
    final val Sleep           = 13
    final val Supervise       = 14
    final val Terminate       = 15
    final val Supervisor      = 16
    final val Run             = 17
  }
  final class FlatMap[E, A0, A] private[IO] (val io: IO[E, A0], val flatMapper: A0 => IO[E, A]) extends IO[E, A] {
    override def tag = Tags.FlatMap
  }

  final class Point[E, A] private[IO] (val value: () => A) extends IO[E, A] {
    override def tag = Tags.Point
  }

  final class Strict[E, A] private[IO] (val value: A) extends IO[E, A] {
    override def tag = Tags.Strict
  }

  final class SyncEffect[E, A] private[IO] (val effect: () => A) extends IO[E, A] {
    override def tag = Tags.SyncEffect
  }

  final class Fail[E, A] private[IO] (val error: E) extends IO[E, A] {
    override def tag = Tags.Fail
  }

  final class AsyncEffect[E, A] private[IO] (val register: (ExitResult[E, A] => Unit) => Async[E, A]) extends IO[E, A] {
    override def tag = Tags.AsyncEffect
  }

  final class AsyncIOEffect[E, A] private[IO] (val register: (ExitResult[E, A] => Unit) => IO[E, Unit])
      extends IO[E, A] {
    override def tag = Tags.AsyncIOEffect
  }

  final class Attempt[E1, E2, A] private[IO] (val value: IO[E1, A]) extends IO[E2, E1 \/ A] {
    override def tag = Tags.Attempt
  }

  final class Fork[E1, E2, A] private[IO] (val value: IO[E1, A], val handler: Maybe[Throwable => IO[Void, Unit]])
      extends IO[E2, Fiber[E1, A]] {
    override def tag = Tags.Fork
  }

  final class Race[E, A0, A1, A] private[IO] (val left: IO[E, A0],
                                              val right: IO[E, A1],
                                              val finishLeft: (A0, Fiber[E, A1]) => IO[E, A],
                                              val finishRight: (A1, Fiber[E, A0]) => IO[E, A])
      extends IO[E, A] {
    override def tag = Tags.Race
  }

  final class Suspend[E, A] private[IO] (val value: () => IO[E, A]) extends IO[E, A] {
    override def tag = Tags.Suspend
  }

  final class Bracket[E, A, B] private[IO] (val acquire: IO[E, A],
                                            val release: (ExitResult[E, B], A) => IO[Void, Unit],
                                            val use: A => IO[E, B])
      extends IO[E, B] {
    override def tag = Tags.Bracket
  }

  final class Uninterruptible[E, A] private[IO] (val io: IO[E, A]) extends IO[E, A] {
    override def tag = Tags.Uninterruptible
  }

  final class Sleep[E] private[IO] (val duration: Duration) extends IO[E, Unit] {
    override def tag = Tags.Sleep
  }

  final class Supervise[E, A] private[IO] (val value: IO[E, A], val error: Throwable) extends IO[E, A] {
    override def tag = Tags.Supervise
  }

  final class Terminate[E, A] private[IO] (val cause: Throwable) extends IO[E, A] {
    override def tag = Tags.Terminate
  }

  final class Supervisor[E] private[IO] () extends IO[E, Throwable => IO[Void, Unit]] {
    override def tag = Tags.Supervisor
  }

  final class Run[E1, E2, A] private[IO] (val value: IO[E1, A]) extends IO[E2, ExitResult[E1, A]] {
    override def tag = Tags.Run
  }

  /**
   * Lifts a strictly evaluated value into the `IO` monad.
   */
  final def now[E, A](a: A): IO[E, A] = new Strict(a)

  /**
   * Lifts a non-strictly evaluated value into the `IO` monad. Do not use this
   * function to capture effectful code. The result is undefined but may
   * include duplicated effects.
   */
  final def point[E, A](a: => A): IO[E, A] = new Point(() => a)

  /**
   * Creates an `IO` value that represents failure with the specified error.
   * The moral equivalent of `throw` for pure code.
   */
  final def fail[E, A](error: E): IO[E, A] = new Fail(error)

  /**
   * Strictly-evaluated unit lifted into the `IO` monad.
   */
  final def unit[E]: IO[E, Unit] = Unit.asInstanceOf[IO[E, Unit]]

  /**
   * Sleeps for the specified duration. This is always asynchronous.
   */
  final def sleep[E](duration: Duration): IO[E, Unit] = new Sleep(duration)

  /**
   * Supervises the specified action, which ensures that any actions directly
   * forked by the action are killed with the specified error upon the action's
   * own termination.
   */
  final def supervise[E, A](io: IO[E, A], error: Throwable): IO[E, A] = new Supervise(io, error)

  /**
   * Flattens a nested action.
   */
  final def flatten[E, A](io: IO[E, IO[E, A]]): IO[E, A] = io.flatMap(a => a)

  /**
   * Lazily produces an `IO` value whose construction may have actional costs
   * that should be be deferred until evaluation.
   *
   * Do not use this method to effectfully construct `IO` values. The results
   * will be undefined and most likely involve the physical explosion of your
   * computer in a heap of rubble.
   */
  final def suspend[E, A](io: => IO[E, A]): IO[E, A] = new Suspend(() => io)

  /**
   * Terminates the fiber executing this action, running all finalizers.
   */
  final def terminate[E, A](t: Throwable): IO[E, A] = new Terminate(t)

  /**
   * Imports a synchronous effect into a pure `IO` value.
   *
   * {{{
   * val nanoTime: IO[Void, Long] = IO.sync(System.nanoTime())
   * }}}
   */
  final def sync[E, A](effect: => A): IO[E, A] = new SyncEffect(() => effect)

  /**
   *
   * Imports a synchronous effect into a pure `IO` value, translating any
   * throwables into a `Throwable` failure in the returned value.
   *
   * {{{
   * def putStrLn(line: String): IO[Throwable, Unit] = IO.syncThrowable(println(line))
   * }}}
   */
  final def syncThrowable[A](effect: => A): IO[Throwable, A] =
    syncCatch(effect) {
      case t: Throwable => t
    }

  /**
   *
   * Imports a synchronous effect into a pure `IO` value, translating any
   * exceptions into an `Exception` failure in the returned value.
   *
   * {{{
   * def putStrLn(line: String): IO[Exception, Unit] = IO.syncException(println(line))
   * }}}
   */
  final def syncException[A](effect: => A): IO[Exception, A] =
    syncCatch(effect) {
      case e: Exception => e
    }

  /**
   * Safely imports an exception-throwing synchronous effect into a pure `IO`
   * value, translating the specified throwables into `E` with the provided
   * user-defined function.
   */
  final def syncCatch[E, A](effect: => A)(f: PartialFunction[Throwable, E]): IO[E, A] =
    IO.absolve(IO.sync(try {
      val result = effect

      Disjunction.right(result)
    } catch { case t: Throwable if f.isDefinedAt(t) => Disjunction.left(f(t)) }))

  /**
   * Imports an asynchronous effect into a pure `IO` value. See `async0` for
   * the more expressive variant of this function.
   */
  final def async[E, A](register: (ExitResult[E, A] => Unit) => Unit): IO[E, A] =
    new AsyncEffect(callback => {
      register(callback)

      Async.later[E, A]
    })

  /**
   * Imports an asynchronous effect into a pure `IO` value. This formulation is
   * necessary when the effect is itself expressed in terms of `IO`.
   */
  final def asyncPure[E, A](register: (ExitResult[E, A] => Unit) => IO[E, Unit]): IO[E, A] = new AsyncIOEffect(register)

  /**
   * Imports an asynchronous effect into a pure `IO` value. The effect has the
   * option of returning the value synchronously, which is useful in cases
   * where it cannot be determined if the effect is synchronous or asynchronous
   * until the effect is actually executed. The effect also has the option of
   * returning a canceler, which will be used by the runtime to cancel the
   * asynchronous effect if the fiber executing the effect is interrupted.
   */
  final def async0[E, A](register: (ExitResult[E, A] => Unit) => Async[E, A]): IO[E, A] = new AsyncEffect(register)

  /**
   * Returns a action that will never produce anything. The moral
   * equivalent of `while(true) {}`, only without the wasted CPU cycles.
   */
  final def never[E, A]: IO[E, A] = Never.asInstanceOf[IO[E, A]]

  /**
   * Submerges the error case of a disjunction into the `IO`. The inverse
   * operation of `IO.attempt`.
   */
  final def absolve[E, A](v: IO[E, E \/ A]): IO[E, A] =
    v.flatMap {
      case -\/(e) => IO.fail(e)
      case \/-(a) => IO.now(a)
    }

  /**
   * Retrieves the supervisor associated with the fiber running the action
   * returned by this method.
   */
  def supervisor[E]: IO[E, Throwable => IO[Void, Unit]] = new Supervisor()

  /**
   * Requires that the given `IO[E, Maybe[A]]` contain a value. If there is no
   * value, then the specified error will be raised.
   */
  final def require[E, A](error: E): IO[E, Maybe[A]] => IO[E, A] =
    (io: IO[E, Maybe[A]]) => io.flatMap(Maybe.maybe(IO.fail[E, A](error))(IO.now[E, A]))

  // TODO: Make this fast, generalize from `Unit` to `A: Semigroup`,
  // and use `IList` instead of `List`.
  def forkAll[E2](l: List[IO[E2, Unit]]): IO[E2, Unit] = l match {
    case Nil     => IO.unit[E2]
    case x :: xs => x.fork.toUnit *> forkAll(xs)
  }

  private final val Never: IO[Nothing, Any] =
    IO.async[Nothing, Any] { (k: (ExitResult[Nothing, Any]) => Unit) =>
      }

  private final val Unit: IO[Nothing, Unit] = now(())
}
