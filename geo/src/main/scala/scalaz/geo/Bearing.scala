package scalaz
package geo

sealed trait Bearing {
  val value: Double
}

trait Bearings {
  def bearing(d: Double) = new Bearing {
    val value = d % 360
  }
}

object Bearing {
  import Scalaz._

  implicit def BearingShow: Show[Bearing] = shows(_.value.shows + "°")

  implicit def BearingEqual: Equal[Bearing] = equalBy(_.value)

  implicit def BearingOrder: Order[Bearing] = orderBy(_.value)
}