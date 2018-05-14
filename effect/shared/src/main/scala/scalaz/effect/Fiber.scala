// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.{ Applicative, Monoid }

/**
 * A fiber is a lightweight thread of execution that never consumes more than a
 * whole thread (but may consume much less, depending on contention). Fibers are
 * spawned by forking `IO` actions, which, conceptually at least, runs them
 * concurrently with the parent `IO` action.
 *
 * Fibers can be joined, yielding their result other fibers, or interrupted,
 * which terminates the fiber with a runtime error.
 *
 * Fork-Join Identity: fork >=> join = id
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
trait Fiber[E, A] { self =>

  /**
   * Joins the fiber, with suspends the joining fiber until the result of the
   * fiber has been determined. Attempting to join a fiber that has been or is
   * killed before producing its result will result in a catchable error.
   */
  def join: IO[E, A]

  /**
   * Interrupts the fiber with the specified error. If the fiber has already
   * terminated, either successfully or with error, this will resume
   * immediately. Otherwise, it will resume when the fiber has been
   * successfully interrupted or has produced its result.
   */
  def interrupt[E2](t: Throwable): IO[E2, Unit]

  /**
   * Monoidal composition of fibers.
   */
  final def <>(that: => Fiber[E, A])(implicit M: Monoid[A]): Fiber[E, A] =
    zipWith(that)(M.append(_, _))

  /**
   * Zips this fiber with the specified fiber, combining their results using
   * the specified combiner function. Both joins and interruptions are performed
   * in sequential order from left to right.
   */
  final def zipWith[B, C](that: => Fiber[E, B])(f: (A, B) => C): Fiber[E, C] =
    new Fiber[E, C] {
      def join: IO[E, C] =
        self.join.zipWith(that.join)(f)

      def interrupt[E2](t: Throwable): IO[E2, Unit] =
        self.interrupt(t) *> that.interrupt(t)
    }
}

object Fiber extends FiberInstances {
  final def empty[E, A](implicit M: Monoid[A]): Fiber[E, A] = Monoid[Fiber[E, A]].empty
  final def point[E, A](a: => A): Fiber[E, A]               = Applicative[Fiber[E, ?]].pure(a)
}
