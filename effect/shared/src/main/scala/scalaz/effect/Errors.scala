// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.data.Disjunction._
import scala.concurrent.duration.Duration

object Errors {
  final case class TimeoutException(duration: Duration) extends
    Exception("The action timed out: " + duration)

  final case class LostRace(loser: Fiber[_] \/ Fiber[_]) extends
    Exception("Lost a race to " + loser.fold(_ => "right")(_ => "left"))
}
