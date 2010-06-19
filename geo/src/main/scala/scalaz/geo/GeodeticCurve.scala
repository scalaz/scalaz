package scalaz
package geo

sealed trait GeodeticCurve {
  val ellipsoidalDistance: Double
  val azi: Azimuth
  val reverseAzi: Azimuth
}

trait GeodeticCurves {
  def curve(d: Double, a: Azimuth, ra: Azimuth) = new GeodeticCurve {
    val ellipsoidalDistance = d.abs
    val azi = a
    val reverseAzi = ra
  }
}

object GeodeticCurve {
  import Scalaz._

  implicit def GeodeticCurveShow: Show[GeodeticCurve] = shows(c => "[" + c.ellipsoidalDistance.shows + " " + c.azi.shows + " " + c.reverseAzi.shows + "]")

  implicit def GeodeticCurveEqual: Equal[GeodeticCurve] = equalBy(c => (c.ellipsoidalDistance, c.azi, c.reverseAzi))

  implicit def GeodeticCurveOrder: Order[GeodeticCurve] = orderBy(c => (c.ellipsoidalDistance, c.azi, c.reverseAzi))
}