// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

/**
 * A lightweight description of the result of executing a fiber. The result is
 * either completed with a value, failed because of an uncaught error thrown
 * from within the fiber, or forcibly interrupted by catastrphic error or
 * another fiber.
 */
sealed trait FiberResult[E, A] { self =>
  import FiberResult._

  final def succeeded: Boolean = self match {
    case Completed(_) => true
    case Failed(_) => false
    case Interrupted(_) => false
  }

  final def failed: Boolean = !succeeded

  final def fold[Z](completed: A => Z, failed: E => Z, interrupted: Throwable => Z): Z = self match {
    case Completed(v) => completed(v)
    case Failed(e) => failed(e)
    case Interrupted(e) => interrupted(e)
  }
}
object FiberResult {
  final case class Completed[E, A](value: A) extends FiberResult[E, A]
  final case class Failed[E, A](error: E) extends FiberResult[E, A]
  final case class Interrupted[E, A](error: Throwable) extends FiberResult[E, A]
}
