// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.data.Maybe

import java.util.concurrent.atomic.AtomicReference

/**
 * A mutable atomic reference for the `IO` monad. This is the `IO` equivalent of
 * a volatile `var`, augmented with atomic operations, which make it useful as a
 * reasonably efficient (if low-level) concurrency primitive.
 *
 * {{{
 * for {
 *   ref <- IORef(2)
 *   v   <- ref.modify(_ + 3)
 *   _   <- putStrLn("Value = " + v.debug) // Value = 5
 * } yield ()
 * }}}
 */
final class IORef[A] private (private val value: AtomicReference[A]) extends AnyVal {

  /**
   * Reads the value from the `IORef`.
   */
  final def read[E]: IO[E, A] = IO.sync(value.get)

  /**
   * Writes a new value to the `IORef`, with a guarantee of immediate
   * consistency (at some cost to performance).
   */
  final def write[E](a: A): IO[E, Unit] = IO.sync(value.set(a))

  /**
   * Writes a new value to the `IORef` without providing a guarantee of
   * immediate consistency.
   */
  final def writeLater[E](a: A): IO[E, Unit] = IO.sync(value.lazySet(a))

  /**
   * Attempts to write a new value to the `IORef`, but aborts immediately under
   * concurrent modification of the value by other fibers.
   */
  final def tryWrite[E](a: A): IO[E, Boolean] = IO.sync(value.compareAndSet(value.get, a))

  /**
   * Atomically modifies the `IORef` with the specified function. This is not
   * implemented in terms of `modifyFold` purely for performance reasons.
   */
  final def modify[E](f: A => A): IO[E, A] = IO.sync {
    var loop    = true
    var next: A = null.asInstanceOf[A]

    while (loop) {
      val current = value.get

      next = f(current)

      loop = !value.compareAndSet(current, next)
    }

    next
  }

  /**
   * Atomically modifies the `IORef` with the specified function, which computes
   * a return value for the modification. This is a more powerful version of
   * `modify`.
   */
  final def modifyFold[E, B](f: A => (B, A)): IO[E, B] = IO.sync {
    var loop = true
    var b: B = null.asInstanceOf[B]

    while (loop) {
      val current = value.get

      val tuple = f(current)

      b = tuple._1

      loop = !value.compareAndSet(current, tuple._2)
    }

    b
  }

  /**
   * Compares and sets the value of the `IORef` if and only if it is `eq` to the
   * specified value. Returns whether or not the ref was modified.
   */
  final def compareAndSet[E](prev: A, next: A): IO[E, Boolean] =
    IO.sync(value.compareAndSet(prev, next))
}

object IORef {

  /**
   * Creates a new `IORef` with the specified value.
   */
  final def apply[E, A](a: A): IO[E, IORef[A]] = IO.sync(new IORef[A](new AtomicReference(a)))
}
