// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.Void

/**
 * The `Async` class describes the return value of an asynchronous effect
 * that is imported into an `IO` value.
 *
 * Asynchronous effects can return `later`, which represents an uninterruptible
 * asynchronous action, `now` which represents a synchronously computed value,
 * `maybeLater`, which represents an interruptible asynchronous action or `maybeLaterIO`
 * which represents an interruptible asynchronous action where the canceler has the
 * form `Throwable => IO[Void, Unit]`
 */
sealed abstract class Async[E, A]
object Async {

  val NoOpCanceler: Canceler = _ => ()
  val NoOpPureCanceler: PureCanceler = _ => IO.unit[Void]

  // TODO: Optimize this common case to less overhead with opaque types
  final case class Now[E, A](value: ExitResult[E, A])         extends Async[E, A]
  final case class MaybeLater[E, A](canceler: Canceler)       extends Async[E, A]
  final case class MaybeLaterIO[E, A](canceler: PureCanceler) extends Async[E, A]

  /**
   * Constructs an `Async` that represents an uninterruptible asynchronous
   * action. The action should invoke the callback passed to the handler when
   * the value is available or the action has failed.
   *
   * See `IO.async0` for more information.
   */
  final def later[E, A]: Async[E, A] =
    MaybeLater(NoOpCanceler)

  /**
   * Constructs an `Async` that represents a synchronous return. The
   * handler should never invoke the callback.
   *
   * See `IO.async0` for more information.
   */
  final def now[E, A](result: ExitResult[E, A]): Async[E, A] = Now(result)

  /**
   * Constructs an `Async` that represents an interruptible asynchronous
   * action. The action should invoke the callback passed to the handler when
   * the value is available or the action has failed.
   *
   * The specified canceler, which must be idempotent, should attempt to cancel
   * the asynchronous action to avoid wasting resources for an action whose
   * results are no longer needed because the fiber computing them has been
   * terminated.
   */
  final def maybeLater[E, A](canceler: Canceler): Async[E, A] =
    MaybeLater(canceler)

  final def maybeLaterIO[E, A](canceler: PureCanceler): Async[E, A] =
    MaybeLaterIO(canceler)
}
