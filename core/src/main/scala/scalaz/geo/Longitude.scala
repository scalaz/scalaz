package scalaz.geo

sealed trait Longitude {
  val longitude: Double
}

trait Longitudes {
  def longitude(d: Double) = new Longitude {
    val longitude = (d + 180) % 360 - 180
  }
}