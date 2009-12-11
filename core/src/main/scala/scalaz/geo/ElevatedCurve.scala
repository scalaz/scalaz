package scalaz.geo

sealed trait ElevatedCurve {
  val curve: GeodeticCurve
  val elevation: Elevation

  lazy val length = {
    val d = curve.ellipsoidalDistance
    val e = elevation.value
    Math.sqrt(d * d + e * e) 
  }
}

trait ElevatedCurves {
  def elevatedCurve(c: GeodeticCurve, e: Elevation) = new ElevatedCurve {
    val curve = c
    val elevation = e
  }
}