package scalaz.database.sql

sealed trait Relation {
  def toSQL: String
}

object Relation {
  implicit def relation(s: String) = new Relation {
    def toSQL = s
  }
}
