package scalaz.geo

sealed trait Elevation {
  val elevation: Double
}

trait Elevations {
  def elevation(d: Double) = new Elevation {
    val elevation = d.abs
  }
}