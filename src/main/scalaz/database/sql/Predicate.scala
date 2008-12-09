package scalaz.database.sql

sealed trait Predicate {
  def toSQL: String

  import Predicate.predicate

  def |||(p: Predicate) = predicate("(" + toSQL + " OR " + p.toSQL + ")")

  def &&&(p: Predicate) = predicate("(" + toSQL + " AND " + p.toSQL + ")") 
}

object Predicate {
  implicit def predicate(s: String) = new Predicate {
    def toSQL = s
  }
}
