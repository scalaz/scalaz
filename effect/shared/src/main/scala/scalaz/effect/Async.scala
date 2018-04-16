// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

/**
 * The `Async` class describes the return value of an asynchronous effect
 * that is imported into an `IO` value.
 *
 * Asynchronous effects can return `later`, which represents an uninterruptible
 * asynchronous action, `now` which represents a synchronously computed value,
 * or `maybeLater`, which represents an interruptible asynchronous action.
 */
sealed abstract class Async[E, A]
object Async {
  type Interruptor = Throwable => Unit

  sealed abstract case class Later[E, A] private () extends Async[E, A]
  object Later {
    // @xuwei-k's trick with a fix by @hrhino. We eliminate allocation overhead
    // but also preserve exhaustivity checking.
    private[this] final class Later_[+E, +A] extends Later[E, A]
    private[this] val value: Later[Nothing, Nothing] = new Later_[Nothing, Nothing]
    def apply[E, A](): Async[E, A]                   = value.asInstanceOf[Later[E, A]]
  }
  // TODO: Optimize this common case to less overhead with opaque types
  final case class Now[E, A](value: ExitResult[E, A])         extends Async[E, A]
  final case class MaybeLater[E, A](interruptor: Interruptor) extends Async[E, A]

  /**
   * Constructs an `Async` that represents an uninterruptible asynchronous
   * action. The action should invoke the callback passed to the handler when
   * the value is available or the action has failed.
   *
   * See `IO.async0` for more information.
   */
  final def later[E, A]: Async[E, A] = Later()

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
  final def maybeLater[E, A](interruptor: Interruptor): Async[E, A] =
    MaybeLater(interruptor)
}
