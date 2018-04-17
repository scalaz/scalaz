// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

/**
 * A description of the result of executing an `IO` value. The result is either
 * completed with a value, failed because of an uncaught `E`, or terminated
 * due to interruption or runtime error.
 */
sealed trait ExitResult[E, A] { self =>
  import ExitResult._

  final def succeeded: Boolean = self match {
    case Completed(_)  => true
    case Failed(_)     => false
    case Terminated(_) => false
  }

  final def failed: Boolean = !succeeded

  final def fold[Z](completed: A => Z, failed: E => Z, interrupted: Throwable => Z): Z = self match {
    case Completed(v)  => completed(v)
    case Failed(e)     => failed(e)
    case Terminated(e) => interrupted(e)
  }
}
object ExitResult {
  final case class Completed[E, A](value: A)          extends ExitResult[E, A]
  final case class Failed[E, A](error: E)             extends ExitResult[E, A]
  final case class Terminated[E, A](error: Throwable) extends ExitResult[E, A]
}
