package scalaz.database.sql

sealed trait Predicate {
  def toSQL: String
}

object Predicate {
  implicit def predicate(s: String) = new Predicate {
    def toSQL = s
  }
}
