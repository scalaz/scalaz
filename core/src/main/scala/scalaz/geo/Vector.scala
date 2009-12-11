package scalaz.geo

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