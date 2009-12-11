package scalaz.geo

sealed trait ElevatedCurve {
  val curve: GeodeticCurve
  val elevation: Elevation
}

trait ElevatedCurves {
  def elevatedCurve(c: GeodeticCurve, e: Elevation) = new ElevatedCurve {
    val curve = c
    val elevation = e
  }
}