package scalaz
package geo

sealed trait Latitude {
  val value: Double

  import Scalaz._
  
  def |:|(lon: Longitude) = coord(this, lon)
}

trait Latitudes {
  def latitude(d: Double) = new Latitude {
    val value = (d + 90) % 180 - 90
  }
}