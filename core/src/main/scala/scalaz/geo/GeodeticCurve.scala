package scalaz.geo

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