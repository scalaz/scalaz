package scalaz
package geo


sealed trait GeoDoubleW extends PimpedType[Double] {
  import Scalaz._
  import Geo._  

  def fromRadians[A](implicit r: Radians[A]) = r.fromRadians(value)

  def |-|(lon: Double) = latitude(value) |:| longitude(lon)

  def rad(lon: Double) = value.fromRadians[Latitude] |:| lon.fromRadians[Longitude]
}


trait GeoDoubleWs {
  implicit def GeoDoubleTo(n: Double): GeoDoubleW = new GeoDoubleW {
    val value = n
  }
}