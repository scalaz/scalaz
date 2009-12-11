package scalaz.geo

sealed trait Coord {
  val latitude: Latitude
  val longitude: Longitude
}

trait Coords {
  def coord(lat: Latitude, lon: Longitude) = new Coord {
    val latitude = lat
    val longitude = lon
  }
}