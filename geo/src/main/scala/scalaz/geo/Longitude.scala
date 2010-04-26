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
  import Predef.{implicitly => i}
  import Scalaz._

  implicit def LongitudeShow: Show[Longitude] = shows(_.value.shows + "°")

  implicit def LongitudeEqual: Equal[Longitude] = i[Equal[Double]] ∙ (_.value)

  implicit def LongitudeOrder: Order[Longitude] = i[Order[Double]] ∙ (_.value)
}