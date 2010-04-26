package scalaz
package geo

sealed trait GeoIdentity[A] extends PimpedType[A] {
  def toRadians(implicit r: Radians[A]): Double = r.toRadians(value)
}

trait GeoIdentitys {
  implicit def GeoIdentityTo[A](x: A) = new GeoIdentity[A] {
    val value = x
  }
}