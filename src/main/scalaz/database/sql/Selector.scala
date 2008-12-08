package scalaz.database.sql

sealed trait Selector {
  def toSQL: String
}

object Selector {
  implicit def selector(s: String) = new Selector {
    def toSQL = s
  }
}
