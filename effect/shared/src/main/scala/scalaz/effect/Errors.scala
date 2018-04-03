// Copyright (C) 2017-2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.data.Disjunction._
import scala.concurrent.duration.Duration

object Errors {
  final case class TimeoutException(duration: Duration) extends
    Exception("The action timed out: " + duration)

  final case class LostRace(loser: Fiber[_, _] \/ Fiber[_, _]) extends
    Exception("Lost a race to " + loser.fold(_ => "right")(_ => "left"))

  final case class InterruptedException(value: Any) extends
    Exception("The action was interrupted due to error: " + value.toString())

  final case class UnhandledError(error: Any) extends
    Exception("An error was not handled by a fiber: " + error)
}
