package scalaz
package geo

sealed trait Azimuth {
  val value: Double
}

trait Azimuths {
  def azimuth(d: Double) = new Azimuth {
    val value = d % 360
  }
}

object Azimuth {
  import Predef.{implicitly => i}
  import Scalaz._

  implicit def AzimuthShow: Show[Azimuth] = shows(_.value.shows + "°")

  implicit def AzimuthEqual: Equal[Azimuth] = i[Equal[Double]] ∙ (_.value)

  implicit def AzimuthOrder: Order[Azimuth] = i[Order[Double]] ∙ (_.value)

}