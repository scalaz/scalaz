package scalaz.database.sql

sealed trait From {
  def relations: List[Relation]

  def select(selector: Selector, selectors: Selector*) = Select.select(this, selector :: selectors.toList)

  def where(p: Predicate) = Select.select(this, Nil) where p

  def sql = relations.map(_.toSQL).mkString(",")
}

object From {
  def from(rs: List[Relation]) = new From {
    def relations = rs
  }

  implicit def FromDatabaseResultSet(f: From) = Database.query("SELECT * FROM " + f.sql)
}
