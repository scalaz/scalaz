// Copyright (C) 2018 John A. De Goes. All rights reserved.

package scalaz.effect

import java.util.concurrent.atomic.AtomicReference

import Promise.internal._

/**
 * A promise represents an asynchronous variable that can be set exactly once,
 * with the ability for an arbitrary number of fibers to suspend (by calling
 * `get`) and automatically resume when the variable is set.
 *
 * Promises can be used for building primitive actions whose completions
 * require the coordinated action of multiple fibers, and for building
 * higher-level concurrent or asynchronous structures.
 * {{{
 * for {
 *   promise <- Promise.make[Void, Int]
 *   _       <- IO.sleep(1.second).promise.complete(42).fork
 *   value   <- promise.get // Resumes when forked fiber completes promise
 * } yield value
 * }}}
 */
class Promise[E, A] private (private val state: AtomicReference[State[E, A]]) extends AnyVal {

  /**
   * Retrieves the value of the promise, suspending the fiber running the action
   * until the result is available.
   */
  final def get: IO[E, A] =
    IO.async0[E, A](k => {
      var result: AsyncReturn[E, A] = null.asInstanceOf[AsyncReturn[E, A]]
      var retry                     = true

      while (retry) {
        val oldState = state.get

        val newState = oldState match {
          case Pending(joiners) =>
            result = AsyncReturn.maybeLater[E, A](interruptJoiner(k))

            Pending(k :: joiners)
          case s @ Done(value) =>
            result = AsyncReturn.now[E, A](value)

            s
        }

        retry = !state.compareAndSet(oldState, newState)
      }

      result
    })

  /**
   * Completes the promise with the specified value.
   */
  final def complete[E2](a: A): IO[E2, Boolean] = done(ExitResult.Completed[E, A](a))

  /**
   * Fails the promise with the specified error.
   */
  final def error[E2](e: E): IO[E2, Boolean] = done(ExitResult.Failed[E, A](e))

  /**
   * Interrupts the promise with the specified throwable.
   */
  final def interrupt[E2](t: Throwable): IO[E2, Boolean] = done(ExitResult.Terminated[E, A](t))

  /**
   * Completes the promise with the specified result. If the specified promise
   * has already been completed, the method will produce false.
   */
  final def done[E2](r: ExitResult[E, A]): IO[E2, Boolean] =
    IO.flatten(IO.sync {
      var action: IO[E2, Boolean] = null.asInstanceOf[IO[E2, Boolean]]
      var retry                   = true

      while (retry) {
        val oldState = state.get

        val newState = oldState match {
          case Pending(joiners) =>
            action =
              forkAll(joiners.map(k => IO.sync[E2, Unit](k(r)))) *>
                IO.now[E2, Boolean](true)

            Done(r)

          case Done(_) =>
            action = IO.now[E2, Boolean](false)

            oldState
        }

        retry = !state.compareAndSet(oldState, newState)
      }

      action
    })

  // TODO: This is the main bottleneck
  private def forkAll[E2](l: List[IO[E2, Unit]]): IO[E2, Unit] = l match {
    case Nil     => IO.unit[E2]
    case x :: xs => x.fork.toUnit *> forkAll(xs)
  }

  private def interruptJoiner(joiner: ExitResult[E, A] => Unit): Throwable => Unit = (t: Throwable) => {
    var retry = true

    while (retry) {
      val oldState = state.get

      val newState = oldState match {
        case Pending(joiners) =>
          Pending(joiners.filter(j => !j.eq(joiner)))

        case Done(_) =>
          oldState
      }

      retry = !state.compareAndSet(oldState, newState)
    }
  }
}
object Promise {

  /**
   * Makes a new promise.
   */
  final def make[E, A]: IO[E, Promise[E, A]] = make0[E, E, A]

  /**
   * Makes a new promise. This is a more powerful variant that can utilize
   * different error parameters for the returned promise and the creation of the
   * promise.
   */
  final def make0[E1, E2, A]: IO[E1, Promise[E2, A]] =
    IO.sync[E1, Promise[E2, A]] {
      new Promise[E2, A](new AtomicReference[State[E2, A]](new internal.Pending[E2, A](Nil)))
    }

  private[effect] object internal {
    sealed trait State[E, A]
    final case class Pending[E, A](joiners: List[ExitResult[E, A] => Unit]) extends State[E, A]
    final case class Done[E, A](value: ExitResult[E, A])                    extends State[E, A]
  }
}
