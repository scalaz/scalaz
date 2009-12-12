package scalaz
package geo

sealed trait Azimuth {
  val value: Double
}

trait Azimuths {
  def azimuth(d: Double) = new Azimuth {
    val value = d % 360
  }
}