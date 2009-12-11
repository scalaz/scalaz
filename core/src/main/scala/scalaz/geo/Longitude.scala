package scalaz.geo

sealed trait Longitude {
  val value: Double
}

trait Longitudes {
  def longitude(d: Double) = new Longitude {
    val value = (d + 180) % 360 - 180
  }
}