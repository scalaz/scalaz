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
  import Predef.{implicitly => i}
  import Scalaz._

  implicit def VectorShow: Show[Vector] = i[Show[(Coord, Bearing)]] ∙ (((_: Vector).coord) &&& ((_: Vector).bearing))

  implicit def VectorEqual: Equal[Vector] = i[Equal[(Coord, Bearing)]] ∙ (((_: Vector).coord) &&& ((_: Vector).bearing))

  implicit def VectorOrder: Order[Vector] = i[Order[(Coord, Bearing)]] ∙ (((_: Vector).coord) &&& ((_: Vector).bearing))
}