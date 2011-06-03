package scalaz
package geo

sealed trait Longitude {
  val value: Double
}

trait Longitudes {
  def longitude(d: Double) = new Longitude {
    val value = (d + 180) % 360 - 180
  }
}

object Longitude {

  import Show._
  import Equal._
  import Order._
  import *._

  implicit def LongitudeShow: Show[Longitude] = shows(_.value.shows + "°")

  implicit def LongitudeEqual: Equal[Longitude] = equalBy(_.value)

  implicit def LongitudeOrder: Order[Longitude] = orderBy(_.value)
}