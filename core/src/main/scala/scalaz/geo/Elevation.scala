package scalaz.geo

sealed trait Elevation {
  val value: Double
}

trait Elevations {
  def elevation(d: Double) = new Elevation {
    val value = d.abs
  }
}