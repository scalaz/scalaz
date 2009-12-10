package scalaz.geo


sealed trait Ellipsoid {
  val semiMajor: Double // metres
  val semiMinor: Double // metres
  val flattening: Double
  val inverseFlattening: Double // metres
}

trait Ellipsoids {
  def ellipsoid(sMajor: Double, sMinor: Double, f: Double, invF: Double) = new Ellipsoid {
    val semiMajor = sMajor
    val semiMinor = sMinor
    val flattening = f
    val inverseFlattening = invF
  }

  def semiMajorInverseF(sMajor: Double, invF: Double) = {
    val f = 1 / invF
    ellipsoid(sMajor, (1D - f) * sMajor, f, invF)
  }

  def semiMajorFlattening(sMajor: Double, flattening: Double) =
    ellipsoid(sMajor, (1D - flattening) * sMajor, flattening, 1 / flattening)

  lazy val wgs84 = semiMajorInverseF(6378137, 298.257223563)
}