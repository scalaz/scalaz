// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.data.Maybe

/**
 * `MVar` is a primitive useful for communicating between fibers, and they can
 * be used to build locks, queues, and other concurrent primitives.
 *
 * Technically, an `MVar` is a variable that can be empty or full, and can be
 * thought of as a two-sided blocking queue of capacity 1, which blocks on takes
 * when there is nothing to take, and blocks on puts when there is already
 * something there.
 *
 * `MVar` values are asynchronous and do not actually block.
 *
 * {{{
 * for {
 *   v <- IO.emptyMVar[Int] // A new empty `MVar` that holds `Int`
 *   _ <- v.put(3).fork     // In a separate fiber, put 3 into the `MVar`
 *   i <- v.take            // Resumes when the above `put` succeeds
 *   _ <- putStrLn("Value = " + i.toString) // "Value = 3"
 * } yield i
 * }}}
 */
trait MVar[A] {
  /**
   * Peeks to see if the `MVar` contains an value. This method returns
   * immediately and synchronously.
   */
  def peek: IO[Maybe[A]]

  /**
   * Takes a value from the `MVar`. If the `MVar` contains a value, it will be
   * returned immediately. If the `MVar` contains no value, then the take
   * request will be added to a FIFO queue, and the returned `IO` action
   * will not resume until a value has been taken from the `MVar`.
   */
  def take: IO[A]

  /**
   * Reads, but does not take, a value from the `MVar`. If the `MVar` contains a
   * value, it will be returned immediately. If the `MVar` contains no value,
   * then the read request will be added to a FIFO queue, and the returned `IO`
   * action will not resume until a value has been read from the `MVar`.
   */
  def read: IO[A]

  /**
   * Puts a value into this `MVar`. If the `MVar` is empty, then the put request
   * will succeed immediately. If the `MVar` already contains a value, then the
   * put request will be added to a FIFO queue, and the returned `IO`
   * action will not resume until the value has been placed in the `MVar`.
   */
  def put(v: A): IO[Unit]

  /**
   * Attempts to synchronously put a value in the `MVar`, but only succeeds if
   * the `MVar` is empty. The status is reflected in the computed `Boolean`.
   */
  def tryPut(v: A): IO[Boolean]

  /**
   * Attempts to synchronously take a value from the `MVar`, but only succeeds
   * if the `MVar` has a value.
   */
  def tryTake: IO[Maybe[A]]
}
