// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.data.Disjunction._

/**
 * A lightweight description of the result of executing a fiber. The result is
 * either completed with a value, failed because of an uncaught error thrown
 * from within the fiber, or interrupted externally by another fiber.
 */
sealed trait BracketResult[A] { self =>
  import BracketResult._

  final def succeeded: Boolean = self match {
    case Completed(_) => true
    case Failed(_) => false
    case Interrupted(_) => false
  }

  final def failed: Boolean = !succeeded

  final def fold[Z](completed: A => Z, failed: Throwable => Z, interrupted: Throwable => Z): Z = self match {
    case Completed(v) => completed(v)
    case Failed(e) => failed(e)
    case Interrupted(e) => interrupted(e)
  }

  /**
   * Converts the bracket result into a disjunction, which throws away
   * information on the source of errors (interruption or failure).
   */
  final def toDisjunction: Throwable \/ A = self match {
    case BracketResult.Completed(a) => \/-(a)
    case BracketResult.Failed(t) => -\/(t)
    case BracketResult.Interrupted(t) => -\/(t)
  }
}
object BracketResult {
  final case class Completed[A](value: A) extends BracketResult[A]
  final case class Failed[A](error: Throwable) extends BracketResult[A]
  final case class Interrupted[A](error: Throwable) extends BracketResult[A]
}
