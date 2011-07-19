package scalaz
package geo

sealed trait GeoIdentity[A] {
  val value: A

  def toRadians(implicit r: Radians[A]): Double = r.toRadians(value)
}

trait GeoIdentitys {
  implicit def GeoIdentityTo[A](x: A): GeoIdentity[A] = new GeoIdentity[A] {
    val value = x
  }
}