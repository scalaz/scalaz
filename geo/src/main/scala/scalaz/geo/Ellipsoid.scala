package scalaz
package geo


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
  lazy val grs80 = semiMajorInverseF(6378137, 298.257222101)
  lazy val grs67 = semiMajorInverseF(6378137, 298.257223563)
  lazy val ans = semiMajorInverseF(6378160, 298.25)
  lazy val wgs72 = semiMajorInverseF(6378135, 298.26)
  lazy val au1965 = semiMajorInverseF(6378160, 298.25)
  lazy val krasovsky1940 = semiMajorInverseF(6378245, 298.3)
  lazy val international1924 = semiMajorInverseF(6378388, 297)
  lazy val hayford1909 = international1924
  lazy val airy1830 = semiMajorInverseF(6377563.4, 299.32)
  lazy val everest1830 = semiMajorInverseF(6377276.3, 300.8)
  lazy val bessel1841 = semiMajorInverseF(6377397.2, 299.15)
  lazy val clarke1858 = semiMajorInverseF(6378293.645, 294.26)
  lazy val clarke1866 = semiMajorInverseF(6378206.4, 294.98)
  lazy val clarke1880 = semiMajorInverseF(6378249.145, 293.465)
  lazy val sphere = semiMajorFlattening(6371000, 0)
}

object Ellipsoid {
  import Scalaz._
  
  implicit def EllipsoidShow: Show[Ellipsoid] = showBy(e => (e.semiMajor, e.semiMinor, e.flattening, e.inverseFlattening))

  implicit def EllipsoidEqual: Equal[Ellipsoid] = equalBy(e => (e.semiMajor, e.semiMinor, e.
          flattening, e.inverseFlattening))

  implicit def EllipsoidOrder: Order[Ellipsoid] = orderBy(e => (e.semiMajor, e.semiMinor, e.flattening, e.inverseFlattening))
}