// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database.sql

sealed trait Relation {
  def toSQL: String
}

object Relation {
  implicit def relation(s: String) = new Relation {
    def toSQL = s
  }
}
