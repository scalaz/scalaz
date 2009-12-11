package scalaz.geo

sealed trait Bearing {
  val value: Double
}

trait Bearings {
  def bearing(d: Double) = new Bearing {
    val value = d % 360
  }
}