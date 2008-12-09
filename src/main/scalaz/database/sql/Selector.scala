// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database.sql

sealed trait Selector {
  def toSQL: String
}

object Selector {
  implicit def selector(s: String) = new Selector {
    def toSQL = s
  }
}
