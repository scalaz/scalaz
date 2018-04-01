// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scala.annotation.switch
import scala.concurrent.duration._

import scalaz.data.Disjunction._
import scalaz.data.Maybe

/**
 * An `IO[E, A]` ("Eye-Oh of A") is an immutable data structure that describes an
 * effectful action that may fail with an `E`, run forever, or produce a single
 * `A` at some point in the future.
 *
 * `IO` values are ordinary immutable values, and may be used like any other
 * values in purely functional code. Because `IO` values just *describe*
 * effects, which must be interpreted by a separate runtime system, they are
 * entirely pure and do not violate referential transparency.
 *
 * `IO` values can efficiently describe the following classes of effects:
 *
 *  * **Pure Values** &mdash; `IO.apply`
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
  final def map[B](f: A => B): IO[E, B] = (self.tag : @switch) match {
    case IO.Tags.Point =>
      val io = self.asInstanceOf[IO.Point[E, A]]

      IO.Point(() => f(io.value()))

    case IO.Tags.Strict =>
      val io = self.asInstanceOf[IO.Strict[E, A]]

      IO.Strict(f(io.value))

    case IO.Tags.SyncEffect =>
      val io = self.asInstanceOf[IO.SyncEffect[E, A]]

      IO.SyncEffect(() => f(io.effect()))

    case IO.Tags.Fail => self.asInstanceOf[IO[E, B]]

    case _ => IO.FlatMap(self, (a: A) => IO.Strict(f(a)))
  }

  /**
   * Creates a composite action that represents this action followed by another
   * one that may depend on the value produced by this one.
   *
   * {{{
   * val parsed = readFile("foo.txt").flatMap(file => parseFile(file))
   * }}}
   */
  final def flatMap[B](f0: A => IO[E, B]): IO[E, B] = (self.tag : @switch) match {
    case IO.Tags.Fail => self.asInstanceOf[IO[E, B]]
    case _ => IO.FlatMap(self, f0)
  }

  /**
   * Forks this action into its own separate fiber, returning immediately
   * without the value produced by this action.
   *
   * The `Fiber[E, A]` returned by this action can be used to interrupt the forked
   * fiber with some exception, or to join the fiber to "await" its computed
   * value.
   *
   * {{{
   * for {
   *   fiber <- subtask.fork
   *   // Do stuff...
   *   a <- subtask.join
   * } yield a
   * }}}
   */
  final def fork: IO[E, Fiber[E, A]] = IO.Fork(this, Maybe.empty)

  /**
   * A more powerful version of `fork` that allows specifying a handler to be
   * invoked on any exceptions that are not handled by the forked fiber.
   */
  final def fork0(handler: Throwable => IO[E, Unit]): IO[E, Fiber[E, A]] =
    IO.Fork(this, Maybe.just(handler))

  /**
   * Executes both this action and the specified action in parallel,
   * returning a tuple of their results. If either individual action fails,
   * then the returned action will fail.
   *
   * TODO: Replace with optimized primitive.
   */
  final def par[B](that: IO[E, B]): IO[E, (A, B)] =
    self.attempt.raceWith(that.attempt) {
      case -\/((-\/(e), fiberb)) => fiberb.interrupt(e) *> IO.fail(e)
      case -\/((\/-(a), fiberb)) => IO.absolve(fiberb.join).map((b: B) => (a, b))
      case \/-((-\/(e), fibera)) => fibera.interrupt(e) *> IO.fail(e)
      case \/-((\/-(b), fibera)) => IO.absolve(fibera.join).map((a: A) => (a, b))
    }

  /**
   * Races this action with the specified action, returning the first
   * result to produce an `A`, whichever it is. If neither action succeeds,
   * then the action will be terminated with some error.
   */
  final def race(that: IO[E, A]): IO[E, A] = raceWith(that) {
    case -\/((a, fiber)) => fiber.interrupt(Errors.LostRace(\/-(fiber))).const(a)
    case \/-((a, fiber)) => fiber.interrupt(Errors.LostRace(-\/(fiber))).const(a)
  }

  /**
   * Races this action with the specified action, invoking the
   * specified finisher as soon as one value or the other has been computed.
   */
  final def raceWith[B, C](that: IO[E, B])(finish: (A, Fiber[E, B]) \/ (B, Fiber[E, A]) => IO[E, C]): IO[E, C] = IO.Race[E, A, B, C](self, that, finish)

  // final def raceWith[E2, E3, B, C](that: IO[E2, B])(finish: (A, Fiber[E2, B]) \/ (B, Fiber[E, A]) => IO[E3, C]): IO[E3, C] = ???

  /**
   * Executes this action and returns its value, if it succeeds, but
   * otherwise executes the specified action.
   */
  final def orElse(that: => IO[E, A]): IO[E, A] =
    self.attempt.flatMap(_.fold(_ => that)(IO.now))

  // final def leftMap[E2](f: E => E2): IO[E2, A] =
  //   IO.absolve(attempt[Void].flatMap {
  //     case -\/ (e) => -\/ (f(e))
  //     case  \/-(a) =>  \/-(a)
  //   })

  // final def attempt[E2](io: IO[E, A]): IO[E2, E \/ A] = ???
  // final def absolve[A](v: IO[Void, E \/ A]): IO[E, A] = ???
  // final def absolve[A, E2](v: IO[E2, E \/ A]): IO[E \/ E2, A] = ???
  // IO.absolve(io.attempt[Void]) == io

  /**
   * Executes this action, capturing both failure and success and returning
   * the result in a `Disjunction`. This method is useful for recovering from
   * `IO` actions that may fail.
   */
  final def attempt: IO[E, Throwable \/ A] = (self.tag : @switch) match {
    case IO.Tags.Point =>
      val io = self.asInstanceOf[IO.Point[E, A]]

      IO.Point(() => \/-(io.value()))

    case IO.Tags.Strict =>
      val io = self.asInstanceOf[IO.Strict[E, A]]

      IO.Strict(\/-(io.value))

    case IO.Tags.Fail =>
      val io = self.asInstanceOf[IO.Fail[E, A]]

      IO.Strict(-\/(io.failure))

    case _ => IO.Attempt(self)
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
   * If the `release` action fails, then the entire computation will fail even
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
  final def bracket[B](release: A => IO[E, Unit])(use: A => IO[E, B]): IO[E, B] = IO.Bracket(this, (_: BracketResult[E, B], a: A) => release(a), use)

  /**
   * A more powerful version of `bracket` that provides information on whether
   * or not `use` succeeded to the release action.
   */
  final def bracket0[B](release: (BracketResult[E, B], A) => IO[E, Unit])(use: A => IO[E, B]): IO[E, B] = IO.Bracket(this, release, use)

  /**
   * A less powerful variant of `bracket` where the value produced by this
   * action is not needed.
   */
  final def bracket_[B](release: IO[E, Unit])(use: IO[E, B]): IO[E, B] =
    self.bracket(_ => release)(_ => use)

  /**
   * Executes the specified finalizer, whether this action succeeds, fails, or
   * is interrupted.
   */
  final def ensuring(finalizer: IO[E, Unit]): IO[E, A] =
    IO.unit.bracket(_ => finalizer)(_ => self)

  /**
   * Executes the release action only if there was an error.
   */
  final def bracketOnError[B](release: A => IO[E, Unit])(use: A => IO[E, B]): IO[E, B] =
    bracket0((r: BracketResult[E, B], a: A) => r match {
      case BracketResult.Failed(_) => release(a)
      case BracketResult.Interrupted(_) => release(a)
      case _ => IO.unit
    })(use)

  /**
   * Runs the specified cleanup action if this action errors, providing the
   * error to the cleanup action. The cleanup action will not be interrupted.
   */
  final def onError(cleanup: Throwable => IO[E, Unit]): IO[E, A] = IO.unit[E].bracket0((r: BracketResult[E, A], a: Unit) => r match {
    case BracketResult.Failed(e) => cleanup(e)
    case BracketResult.Interrupted(e) => cleanup(e)
    case _ => IO.unit
  })(_ => self)

  /**
   * Supervises this action, which ensures that any fibers that are forked
   * by the action are killed with the specified error when this action
   * completes.
   */
  final def supervised(error: Throwable): IO[E, A] = IO.Supervise(self, error)

  /**
   * Performs this action non-interruptibly. This will prevent the
   * action from being killed externally, but the action may fail
   * for internal reasons (e.g. an uncaught exception).
   */
  final def uninterruptibly: IO[E, A] = IO.Uninterruptible(self)

  /**
   * Recovers from all errors.
   *
   * {{{
   * openFile("config.json").catchAll(_ => IO.now(defaultConfig))
   * }}}
   */
  final def catchAll(h: Throwable => IO[E, A]): IO[E, A] = catchSome {
    case t : Throwable => h(t)
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
  final def catchSome(pf: PartialFunction[Throwable, IO[E, A]]): IO[E, A] = {
    def tryRescue(t: Throwable): IO[E, A] =
      if (pf.isDefinedAt(t)) pf(t) else IO.fail(t)

    self.attempt.flatMap(_.fold(tryRescue)(IO.now))
  }

  /**
   * Maps this action to the specified constant while preserving the
   * effects of this action.
   */
  final def const[B](b: => B): IO[E, B] = self.map(_ => b)

  /**
   * A variant of `flatMap` that ignores the value produced by this action.
   */
  final def *> [B](io: => IO[E, B]): IO[E, B] = self.flatMap(_ => io)

  /**
   * Sequences the specified action after this action, but ignores the
   * value produced by the action.
   */
  final def <* [B](io: => IO[E, B]): IO[E, A] = self.flatMap(io.const(_))

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
  final def retryFor(duration: Duration): IO[E, A] =
    IO.absolve(
      retry.attempt race (IO.sleep(duration) *>
                          IO.now(-\/(Errors.TimeoutException(duration)))))

  /**
   * Retries continuously, increasing the duration between retries each time by
   * the specified multiplication factor, and stopping after the specified upper
   * limit on retries.
   */
  final def retryBackoff(n: Int, factor: Double, duration: Duration): IO[E, A] =
    if (n <= 1) self
    else self orElse (IO.sleep(duration) *> retryBackoff(n - 1, factor, duration * factor))

  /**
   * Repeats this computation continuously until the first error, with the
   * specified interval between each full execution.
   */
  final def repeat[B](interval: Duration): IO[E, B] =
    self *> IO.sleep(interval) *> repeat(interval)

  /**
   * Repeats this computation continuously until the first error, with the
   * specified interval between the start of each full execution. Note that if
   * the execution of this computation takes longer than the specified interval,
   * then the computation will instead execute as quickly as possible, but not
   * necessarily at the specified interval.
   */
  final def repeatFixed[B](interval: Duration): IO[E, B] =
    repeatFixed0(IO.sync(System.nanoTime()))(interval)

  final def repeatFixed0[B](nanoTime: IO[E, Long])(interval: Duration): IO[E, B] =
    IO.flatten(nanoTime.flatMap { start =>
      val gapNs = interval.toNanos

      def tick[B](n: Int): IO[E, B] =
        self *> nanoTime.flatMap { now =>
          val await = ((start + n * gapNs) - now).max(0L)

          IO.sleep(await.nanoseconds) *> tick[B](n + 1)
        }

      tick(1)
    })

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
  final def timeout(duration: Duration): IO[E, A] = {
    val err: IO[E, Throwable \/ A] = IO.now(-\/(Errors.TimeoutException(duration)))

    IO.absolve(self.attempt.race(err.delay(duration)))
  }

  /**
   * Returns a new computation that executes this one and times the execution.
   */
  final def timed: IO[E, (Duration, A)] = timed0(IO.sync(System.nanoTime()))

  /**
   * A more powerful variation of `timed` that allows specifying the clock.
   */
  final def timed0(nanoTime: IO[E, Long]): IO[E, (Duration, A)] =
    summarized[Long, Duration]((start, end) => Duration.fromNanos(end - start))(nanoTime)

  /**
   * Summarizes a computation by computing some value before and after
   * execution, and then combining the values to produce a summary, together
   * with the result of execution.
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
   * An integer that identifies the term in the `IO` sum type to which this
   * instance belongs (e.g. `IO.Tags.Point`).
   */
  def tag: Int
}

object IO extends IOInstances {
  object Tags {
    final val FlatMap           = 0
    final val Point             = 1
    final val Strict            = 2
    final val SyncEffect        = 3
    final val Fail              = 4
    final val AsyncEffect       = 5
    final val Attempt           = 6
    final val Fork              = 7
    final val Race              = 8
    final val Suspend           = 9
    final val Bracket           = 10
    final val Uninterruptible   = 11
    final val Sleep             = 12
    final val Supervise         = 13
    final val Interrupt         = 14
  }
  final case class FlatMap[E, A0, A](io: IO[E, A0], flatMapper: A0 => IO[E, A]) extends IO[E, A] {
    override final def tag = Tags.FlatMap
  }

  final case class Point[E, A](value: () => A) extends IO[E, A] {
    override final def tag = Tags.Point
  }

  final case class Strict[E, A](value: A) extends IO[E, A] {
    override final def tag = Tags.Strict
  }

  final case class SyncEffect[E, A](effect: () => A) extends IO[E, A] {
    override final def tag = Tags.SyncEffect
  }

  final case class Fail[E, A](failure: Throwable) extends IO[E, A] {
    override final def tag = Tags.Fail
  }

  final case class AsyncEffect[E, A](register: (BracketResult[E, A] => Unit) => AsyncReturn[A]) extends IO[E, A] {
    override final def tag = Tags.AsyncEffect
  }

  final case class Attempt[E, A](value: IO[E, A]) extends IO[E, Throwable \/ A] {
    override final def tag = Tags.Attempt
  }

  final case class Fork[E, A](value: IO[E, A], handler: Maybe[Throwable => IO[E, Unit]]) extends IO[E, Fiber[E, A]] {
    override final def tag = Tags.Fork
  }

  final case class Race[E, A0, A1, A](left: IO[E, A0], right: IO[E, A1], finish: (A0, Fiber[E, A1]) \/ (A1, Fiber[E, A0]) => IO[E, A]) extends IO[E, A] {
    override final def tag = Tags.Race
  }

  final case class Suspend[E, A](value: () => IO[E, A]) extends IO[E, A] {
    override final def tag = Tags.Suspend
  }

  final case class Bracket[E, A, B](acquire: IO[E, A], release: (BracketResult[E, B], A) => IO[E, Unit], use: A => IO[E, B]) extends IO[E, B] {
    override final def tag = Tags.Bracket
  }

  final case class Uninterruptible[E, A](io: IO[E, A]) extends IO[E, A] {
    override final def tag = Tags.Uninterruptible
  }

  final case class Sleep[E](duration: Duration) extends IO[E, Unit] {
    override final def tag = Tags.Sleep
  }

  final case class Supervise[E, A](value: IO[E, A], error: Throwable) extends IO[E, A] {
    override final def tag = Tags.Supervise
  }

  final case class Interrupt[E, A](failure: Throwable) extends IO[E, A] {
    override final def tag = Tags.Interrupt
  }

  /**
   * Lifts a strictly evaluated value into the `IO` monad.
   */
  final def now[E, A](a: A): IO[E, A] = Strict(a)

  /**
   * Lifts a non-strictly evaluated value into the `IO` monad. Do not use this
   * function to capture effectful code. The result is undefined but may
   * include runtime errors.
   */
  final def point[E, A](a: => A): IO[E, A] = Point(() => a)

  /**
   * Raises the specified error. The moral equivalent of `throw` for pure code.
   */
  final def fail[E, A](t: Throwable): IO[E, A] = Fail(t)

  // final def fail[E, A](t: E): IO[E, A] = Fail(t)

  /**
   * Strictly-evaluated unit lifted into the `IO` monad.
   */
  def unit[E]: IO[E, Unit] = Unit.asInstanceOf[IO[E, Unit]]

  private val Unit: IO[Nothing, Unit] = now(())

  /**
   * Sleeps for the specified duration. This is always asynchronous.
   */
  final def sleep[E](duration: Duration): IO[E, Unit] = Sleep(duration)

  /**
   * Supervises the specified action, which ensures that any actions directly
   * forked by the action are killed with the specified error upon the action's
   * own termination.
   */
  final def supervise[E, A](io: IO[E, A], error: Throwable): IO[E, A] = Supervise(io, error)

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
  final def suspend[E, A](io: => IO[E, A]): IO[E, A] = Suspend(() => io)

  /**
   * Imports a synchronous effect into a pure `IO` value. If the thunk returns
   * a null value, this will throw a `NullPointerException` inside `IO`.
   *
   * {{{
   * def putStrLn(line: String): IO[Throwable, Unit] = IO.sync(println(line))
   * }}}
   */
  final def sync[E, A](effect: => A): IO[E, A] = SyncEffect(() => effect)

  // final def sync[A](effect: => A): IO[Throwable, A] = ???
  //final def sync[E, A](effect: => A): IO[E, A] = SyncEffect(() => effect)

  /**
   * Imports an asynchronous effect into a pure `IO` value. See `async0` for
   * the more expressive variant of this function.
   */
  final def async[E, A](register: (BracketResult[E, A] => Unit) => Unit): IO[E, A] = AsyncEffect { callback =>
    register(callback)

    AsyncReturn.later[A]
  }

  /**
   * Imports an asynchronous effect into a pure `IO` value. The effect has the
   * option of returning the value synchronously, which is useful in cases
   * where it cannot be determined if the effect is synchronous or asynchronous
   * until the effect is actually executed. The effect also has the option of
   * returning a canceler, which will be used by the runtime to cancel the
   * asynchronous effect if the fiber executing the effect is interrupted.
   */
  final def async0[E, A](register: (BracketResult[E, A] => Unit) => AsyncReturn[A]): IO[E, A] = AsyncEffect(register)

  /**
   * Returns a computation that will never produce anything. The moral
   * equivalent of `while(true) {}`, only without the wasted CPU cycles.
   */
  final def never[E, A]: IO[E, A] = Never.asInstanceOf[IO[E, A]]

  /**
   * Submerges the error case of a disjunction into the `IO`. The inverse
   * operation of `IO.attempt`.
   */
  final def absolve[E, A](v: IO[E, Throwable \/ A]): IO[E, A] =
    v.flatMap {
      case -\/(e) => IO.fail(e)
      case \/-(a) => IO.now(a)
    }

  // Add:
  // def uncaughtErrorHandler[E]: IO[E, Throwable => IO[Unit]]
  //

  // final def absolve[A](v: IO[Void, E \/ A]): IO[E, A] = ???

  /**
   * Requires that the given `IO[E, Maybe[A]]` contain a value. If there is no
   * value, then the specified error will be raised.
   */
  final def require[E, A](t: Throwable): IO[E, Maybe[A]] => IO[E, A] =
    (io: IO[E, Maybe[A]]) => io.flatMap(Maybe.maybe(IO.fail[E, A](t))(IO.now[E, A](_)))

  private final val Never: IO[Nothing, Any] = IO.async[Nothing, Any] { (k: (BracketResult[Nothing, Any]) => Unit) => }
}
