package scalaz.geo

sealed trait Bearing {
  val bearing: Double
}

trait Bearings {
  def bearing(d: Double) = new Bearing {
    val bearing = d % 360
  }
}