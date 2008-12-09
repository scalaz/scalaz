// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database.sql

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait From {
  def relations: List[Relation]

  def select(selector: Selector, selectors: Selector*) = Select.select(this, selector :: selectors.toList)

  def where(p: Predicate) = Select.select(this, Nil) where p

  def sql = relations.map(_.toSQL).mkString(",")
}

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */  
object From {
  def from(rs: List[Relation]) = new From {
    def relations = rs
  }

  implicit def FromDatabaseResultSet(f: From) = Database.query("SELECT * FROM " + f.sql)
}
