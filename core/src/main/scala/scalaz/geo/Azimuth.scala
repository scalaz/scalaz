package scalaz.geo

sealed trait Azimuth {
  val azimuth: Double
}

trait Azimuths {
  def azimuth(d: Double) = new Azimuth {
    val azimuth = d % 360
  }
}