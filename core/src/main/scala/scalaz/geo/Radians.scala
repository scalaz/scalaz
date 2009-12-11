package scalaz.geo

sealed trait Radians[A] {
  val toRadians: A => Double
  val fromRadians: Double => A
}

object Radians {
  import scalaz.Scalaz._

  implicit val DoubleRadians: Radians[Double] = radians(x => x, x => x)

  implicit val LatitudeRadians: Radians[Latitude] = radians(_.latitude, latitude(_))

  implicit val LongitudeRadians: Radians[Longitude] = radians(_.longitude, longitude(_))

  implicit val BearingRadians: Radians[Bearing] = radians(_.bearing, bearing(_)) 
}

trait Radianss {
  def radians[A](to: A => Double, from: Double => A) = new Radians[A] {
    val toRadians = to
    val fromRadians = from
  }
}