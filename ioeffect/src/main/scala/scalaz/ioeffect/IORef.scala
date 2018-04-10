// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz
package ioeffect

import java.util.concurrent.atomic.AtomicReference

/**
  * A high-performance, mutable reference for the `IO` monad. This is the `IO`
  * equivalent of a volatile `var`, and is useful for mutable references inside
  * a single fiber, not communication between fibers. For communication between
  * fibers, see `MVar`.
  *
  * {{{
  * for {
  *   ref <- IORef(2)
  *   v   <- ref.modify(_ + 3)
  *   _   <- putStrLn("Value = " + v.debug) // Value = 5
  * } yield ()
  * }}}
  */
final class IORef[A] private (private val value: AtomicReference[A]) {

  /**
    * Reads the value from the `IORef`.
    */
  final def read: IO[A] = IO.sync(value.get())

  /**
    * Writes a new value to the `IORef`.
    */
  final def write(a: A): IO[Unit] = IO.sync(value.set(a))

  /**
    * Modifies the `IORef` with the specified function.
    */
  final def modify(f: A => A): IO[A] = IO.sync {
    val cur = value.get()
    val next = f(cur)
    value.set(next)
    next
  }
}

object IORef {

  /**
    * Creates a new `IORef` with the specified value.
    */
  final def apply[A](a: A): IO[IORef[A]] = IO.sync(new IORef[A](new AtomicReference[A](a)))
}
