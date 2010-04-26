package scalaz
package geo

sealed trait Latitude {
  val value: Double

  import Geo.coord
  
  def |:|(lon: Longitude) = coord(this, lon)
}

trait Latitudes {
  def latitude(d: Double) = new Latitude {
    val value = (d + 90) % 180 - 90
  }
}

object Latitude {
  import Predef.{implicitly => i}
  import Scalaz._

  implicit def LatitudeShow: Show[Latitude] = shows(_.value.shows + "°")

  implicit def LatitudeEqual: Equal[Latitude] = i[Equal[Double]] ∙ (_.value)

  implicit def LatitudeOrder: Order[Latitude] = i[Order[Double]] ∙ (_.value)
}