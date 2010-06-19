package scalaz
package geo

sealed trait Coord {
  val latitude: Latitude
  val longitude: Longitude

  import Scalaz._
  import scala.math._
  import Geo._

  def |*|(e: Elevation) = position(this, e)

  private def square(d: Double) = d * d

  def direct(bear: Bearing, dist: Double, convergence: Double = 0.0000000000001D, ellipsoid: Ellipsoid = wgs84): Vector = {
    case class P(origSigma: Double, sigma: Double, prevSigma: Double) {
      def transition(d: Double) = P(origSigma, origSigma + d, sigma)

      lazy val sinSigma = sin(sigma)

      lazy val cosSigma = cos(sigma)

      def sigmaM2(d: Double) = 2D * d + sigma

      def cosSigmaM2(d: Double) = cos(sigmaM2(d))

      def cos2SigmaM2(d: Double) =
        square(cosSigmaM2(d))
    }

    def ps(d: Double) = P(d, d, d)

    val sMnr = ellipsoid.semiMinor
    val flat = ellipsoid.flattening
    val alpha = bear.toRadians
    val cosAlpha = cos(alpha)
    val sinAlpha = sin(alpha)
    val tanu1 = (1D - flat) * tan(latitude.toRadians)
    val cosu1 = 1D / sqrt(1D + square(tanu1))
    val sinu1 = tanu1 * cosu1
    val sigma1 = atan2(tanu1, cosAlpha)
    val csa = cosu1 * sinAlpha
    val sin2Alpha = csa * csa
    val cos2Alpha = 1 - sin2Alpha
    def ab(d: Double, f: Double, g: Double, h: Double, i: Double) = {
      val s = cos2Alpha * (square(ellipsoid.semiMajor / sMnr) - 1)
      (s / d) * ( f + s * (g + s * (h - i * s)))
    }
    val a = 1 + ab(16384, 4096, -768, 320, 175)
    val b = ab(1024, 256, -128, 74, 47)
    val end = {
      val begin = ps(dist / sMnr / a)
      def iter(p: P) = {
        def tf(d: Double) = -3 + 4 * d
        val cosSigmaM2 = p.cosSigmaM2(sigma1)
        val cos2SigmaM2 = p.cos2SigmaM2(sigma1)
        p.transition(b * p.sinSigma * (cosSigmaM2 + b / 4D * (p.cosSigma * (-1 + 2 * cos2SigmaM2) - (b / 6D) * cosSigmaM2 * tf(square(p.sinSigma)) * tf(cos2SigmaM2))))
      }
      def pred(p: P) = (p.sigma - p.prevSigma).abs >= convergence
      begin.doWhile(iter(_), pred(_))
    }
    val c = flat / 16 * cos2Alpha * (4 + flat * (4 - 3 * cos2Alpha))
    val cc = cosu1 * end.cosSigma
    val ccca = cc * cosAlpha
    val sss = sinu1 * end.sinSigma
    val lat = atan2(sinu1 * end.cosSigma + cosu1 * end.sinSigma * cosAlpha, (1D - flat) * sqrt(sin2Alpha + square(sss - ccca))).fromRadians[Latitude].value
    val lon = longitude.value + (atan2(end.sinSigma * sinAlpha, cc - sss * cosAlpha) - (1 - c) * flat * csa * (end.sigma + c * end.sinSigma * (end.cosSigmaM2(sigma1) + c * end.cosSigma * (-1 + 2 * end.cos2SigmaM2(sigma1))))).fromRadians[Longitude].value
    vector(lat |-| lon, (atan2(csa, ccca - sss)).fromRadians[Bearing])
  }

  def inverse(end: Coord, convergence: Double = 0.0000000000001D, ellipsoid: Ellipsoid = wgs84): GeodeticCurve = {
    sealed trait InverseResult
    case object Continue extends InverseResult
    case object Limit extends InverseResult
    case object Converge extends InverseResult

    case class Q(count: Int, result: InverseResult, lambda: Double, a: Double, sigma: Double, deltasigma: Double)

    val b = ellipsoid.semiMinor
    val f = ellipsoid.flattening
    val (phi1, phi2) = {
      def rl(k: Coord) = k.latitude.toRadians
      (rl(this), rl(end))
    }
    val a2b2b2 = square(ellipsoid.semiMajor) / square(b) - 1
    val omega = {
      def rl(k: Coord) = k.longitude.toRadians
      rl(end) - rl(this)
    }
    val (u1, u2) = {
      def at(d: Double) = atan ((1 - f) * (tan(d)))
      (at(phi1), at(phi2))
    }
    val sinu1 = sin(u1)
    val cosu1 = cos(u1)
    val sinu2 = sin(u2)
    val cosu2 = cos(u2)
    val sinu1sinu2 = sinu1 * sinu2
    val cosu1sinu2 = cosu1 * sinu2
    val sinu1cosu2 = sinu1 * cosu2
    val cosu1cosu2 = cosu1 * cosu2
    val begin = Q(0, Continue, omega, 0, 0, 0)
    def iter(q: Q): Q = {
      val sinlambda = sin(q.lambda)
      val coslambda = cos(q.lambda)
      val sin2sigma = square(cosu2) * square(sinlambda)+ square(cosu1sinu2 - sinu1cosu2 * coslambda)
      val sinsigma = sqrt(sin2sigma)
      val cossigma = sinu1sinu2 + cosu1cosu2 * coslambda
      val sigma = atan2(sinsigma, cossigma)
      val sinalpha = if(sin2sigma == 0D) 0D else cosu1cosu2 * sinlambda / sinsigma
      val alpha = asin(sinalpha)
      val cos2alpha = square(cos(alpha))
      val cos2sigmam = if(cos2alpha == 0D) 0D else cossigma - 2 * sinu1sinu2 / cos2alpha
      val u2 = cos2alpha * a2b2b2
      val cos2sigmam2 = square(cos2sigmam)
      val a = 1D + u2 / 16384 * (4096 + u2 * (u2 * (320 - 175 * u2) - 768))
      val b = u2 / 1024 * (256 + u2 * (u2 * (74 - 47 * u2) - 128))
      val deltasigma = b * sinsigma * (cos2sigmam + b / 4 * (cossigma * (2 * cos2sigmam2 - 1) - b / 6 * cos2sigmam * (4 * sin2sigma - 3) * (cos2sigmam2 * 4 - 3)))
      val c  = f / 16 * cos2alpha * (4 + f * (4 - 3 * cos2alpha))
      val l  = omega + (1 - c) * f * sinalpha * (sigma + c * sinsigma * (cos2sigmam + c * cossigma * (2 * cos2sigmam2 - 1)))
      val r = if(q.count == 20) Limit
              else if(q.count > 1 && cos(alpha) < convergence) Converge
              else Continue
      Q(q.count + 1, r, l, a, sigma, deltasigma)
    }
    def pred(q: Q) = q.result == Continue
    val ed = begin.whileDo(iter(_), pred(_))
    def ifi[A](p: A => Boolean, t: A => A, a: A): A = if(p(a)) t(a) else a

    def normalise(d: Double) = if(d >= 360) d - 360 else d
    val (a1, a2) = if(ed.result == Converge) (0D, 0D)
                           else if(phi1 ≩ phi2) (180D, 0D)
                           else if(phi1 ≨ phi2) (0D, 180D)
                           else (Double.NaN, Double.NaN)
    val (alpha1, alpha2) = (normalise(a1), normalise(a2))
    curve(b * ed.a * (ed.sigma - ed.deltasigma), azimuth(alpha1), azimuth(alpha2))
  }
}

trait Coords {
  def coord(lat: Latitude, lon: Longitude) = new Coord {
    val latitude = lat
    val longitude = lon
  }
}

object Coord {
  import Scalaz._
  import Geo._

  implicit def CoordShow: Show[Coord] = shows((c: Coord) => "[" + c.latitude.shows + " " + c.longitude.shows + "]")

  implicit def CoordEqual: Equal[Coord] = equalBy(((_: Coord).latitude) &&& ((_: Coord).longitude))

  implicit def CoordOrder: Order[Coord] = orderBy(((_: Coord).latitude) &&& ((_: Coord).longitude))
}