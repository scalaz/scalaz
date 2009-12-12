package scalaz
package geo

sealed trait Position {
  val coord: Coord
  val elevation: Elevation
}

trait Positions {
  def position(c: Coord, e: Elevation) = new Position {
    val coord = c
    val elevation = e
  }
}