package scalaz.database.sql

sealed trait Select {
  def from: From
  def selectors: List[Selector]
  def sql = selectors match {
    case Nil => "*"
    case _ => selectors.map(_.toSQL).mkString(",")
  }

  def where(p: Predicate) = Database.query("SELECT " + sql + " FROM " + from.sql + " WHERE " + p.toSQL)
}

object Select {
  def select(f: From, s: List[Selector]) = new Select {
    def from = f
    def selectors = s
  }

  implicit def SelectDatabaseResultSet(s: Select) = Database.query("SELECT " + s.sql + " FROM " + s.from.sql)
}
