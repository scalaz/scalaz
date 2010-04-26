package scalaz
package geo

sealed trait Radians[A] {
  import Geo._
  
  val toRadians: A => Double
  val fromRadians: Double => A

  def xmap[B](f: B => A, g: A => B) = new Radians[B] {
    val toRadians = f andThen Radians.this.toRadians
    val fromRadians = Radians.this.fromRadians andThen g
  }
}

object Radians {
  import Scalaz._
  import Geo._  

  implicit val DoubleRadians: Radians[Double] = radians(_ * π / 180, _ * 180 / π)

  implicit val LatitudeRadians: Radians[Latitude] = DoubleRadians xmap (_.value, latitude(_))

  implicit val LongitudeRadians: Radians[Longitude] = DoubleRadians xmap (_.value, longitude(_))

  implicit val BearingRadians: Radians[Bearing] = DoubleRadians xmap (_.value, bearing(_))
}

trait Radianss {
  def radians[A](to: A => Double, from: Double => A) = new Radians[A] {
    val toRadians = to
    val fromRadians = from
  }
}
