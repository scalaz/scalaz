package scalaz

sealed trait DoubleW {
  val value: Double

  import Scalaz._
  import geo._

  def fromRadians[A](implicit r: Radians[A]) = r.fromRadians(value)

  def |-|(lon: Double) = latitude(value) |:| longitude(lon)

  def rad(lon: Double) = value.fromRadians[Latitude] |:| lon.fromRadians[Longitude]
}

trait DoubleWs {
  implicit def DoubleTo(n: Double): DoubleW = new DoubleW {
    val value = n
  }

  implicit def DoubleFrom(n: DoubleW): Double = n.value
}
