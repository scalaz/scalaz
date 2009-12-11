package scalaz.geo

sealed trait Coord {
  val latitude: Latitude
  val longitude: Longitude

  import scalaz.Scalaz._
  import Math._

  def |*|(e: Elevation) = position(this, e)

  private case class P(origSigma: Double, sigma: Double, prevSigma: Double) {
    def transition(d: Double) = P(origSigma, origSigma + d, sigma)

    lazy val sinSigma = sin(sigma)

    lazy val cosSigma = cos(sigma)

    def sigmaM2(d: Double) = 2D * d + sigma

    def cosSigmaM2(d: Double) = cos(sigmaM2(d))

    def cos2SigmaM2(d: Double) = {
      val k = cosSigmaM2(d)
      k * k
    }
  }

  private def ps(d: Double) = P(d, d, d)
  
  def direct(bear: Bearing, dist: Double, convergence: Double = 0.0000000000001D)(implicit e: Ellipsoid): Vector = {
    def square(d: Double) = d * d
    val sMnr = e.semiMinor
    val flat = e.flattening
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
      val m = e.semiMajor / sMnr
      val s = cos2Alpha * square(m) - 1
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
        p.transition(b * p.sinSigma * (cosSigmaM2 + b / 4D * (p.cosSigma * (-1 + 2 * cos2SigmaM2) - (b / 6D) * cosSigmaM2 * tf(p.sinSigma * p.sinSigma) * tf(square(cos2SigmaM2)))))
      }
      def pred(p: P) = (p.sigma + p.prevSigma).abs >= convergence
      begin.doWhile(iter(_), pred(_))
    }
    val c = flat / 16 * cos2Alpha * (4 + flat * (4 - 3 * cos2Alpha))
    val cc = cosu1 * end.cosSigma
    val ccca = cc * cosAlpha
    val sss = sinu1 * end.sinSigma
    val lat = atan2(sinu1 * end.cosSigma + cosu1 * end.sinSigma * cosAlpha, (1D - flat) * sqrt(sin2Alpha + square(sss - ccca))).fromRadians[Latitude].value
    val lon = longitude.value + (atan2(end.sinSigma * sinAlpha, cc - sss * cosAlpha) - (1 - c) * flat * csa * (end.sigma + c * end.sinSigma * (end.cosSigmaM2(sigma1) + c * end.cosSigma * (-1 + 2 * end.cos2SigmaM2(sigma1))))).fromRadians[Longitude].value
    vector(lat |-| lon, bearing(atan2(csa, ccca - sss)))
  }
}

trait Coords {
  def coord(lat: Latitude, lon: Longitude) = new Coord {
    val latitude = lat
    val longitude = lon
  }
}