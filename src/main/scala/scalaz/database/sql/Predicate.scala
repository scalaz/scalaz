// Copyright Tony Morris 2008-2009
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
sealed trait Predicate {
  def toSQL: String

  import Predicate.predicate

  def |||(p: Predicate) = predicate("(" + toSQL + " OR " + p.toSQL + ")")

  def &&&(p: Predicate) = predicate("(" + toSQL + " AND " + p.toSQL + ")") 
}

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */  
object Predicate {
  implicit def predicate(s: String) = new Predicate {
    def toSQL = s
  }
}
