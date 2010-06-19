package scalaz
package geo

sealed trait Vector {
  val coord: Coord
  val bearing: Bearing
}

trait Vectors {
  def vector(c: Coord, b: Bearing) = new Vector {
    val coord = c
    val bearing = b
  }
}

object Vector {
  import Scalaz._

  implicit def VectorShow: Show[Vector] = showBy(((_: Vector).coord) &&& ((_: Vector).bearing))

  implicit def VectorEqual: Equal[Vector] = equalBy(((_: Vector).coord) &&& ((_: Vector).bearing))

  implicit def VectorOrder: Order[Vector] = orderBy(((_: Vector).coord) &&& ((_: Vector).bearing))
}