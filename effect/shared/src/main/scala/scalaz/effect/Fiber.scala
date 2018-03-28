// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

/**
 * A fiber is a lightweight thread of execution that never consumes more than a
 * whole thread (but may consume much less, depending on contention). Fibers are
 * spawned by forking `IO` actions, which, conceptually at least, runs them
 * concurrently with the parent `IO` action.
 *
 * Fibers can be joined, yielding their result other fibers, or interrupted,
 * which terminates the fiber with a user-defined error.
 *
 * {{{
 * for {
 *   fiber1 <- io1.fork
 *   fiber2 <- io2.fork
 *   _      <- fiber1.interrupt(e)
 *   a      <- fiber2.join
 * } yield a
 * }}}
 */
trait Fiber[E, A] {
  /**
   * Joins the fiber, with suspends the joining fiber until the result of the
   * fiber has been determined. Attempting to join a fiber that has been or is
   * killed before producing its result will result in a catchable error.
   */
  def join: IO[E, A]

  // fork >=> join = id

  // def pause: IO[Void, IO[E, A]]

  /**
   * Interrupts the fiber with the specified error. If the fiber has already
   * terminated, either successfully or with error, this will resume
   * immediately. Otherwise, it will resume when the fiber has been
   * successfully interrupted or has produced its result.
   */
  def interrupt(t: Throwable): IO[E, Unit]

  /**
   * Interrupts the fiber with the specified error asynchronously, and ignores
   * any errors that result from interrupting the fiber.
   */
  final def interruptIgnore(t: Throwable): IO[E, Unit] = interrupt(t).fork.toUnit
}
