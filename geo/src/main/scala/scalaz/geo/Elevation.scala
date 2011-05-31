package scalaz
package geo

sealed trait Elevation {
  val value: Double
}

trait Elevations {
  def elevation(d: Double) = new Elevation {
    val value = d.abs
  }
}

object Elevation {

  import Show._
  import Equal._
  import Order._
  import data.*._

  implicit def ElevationShow: Show[Elevation] = shows(_.value.shows + "m")

  implicit def ElevationEqual: Equal[Elevation] = equalBy(_.value)

  implicit def ElevationOrder: Order[Elevation] = orderBy(_.value)
}