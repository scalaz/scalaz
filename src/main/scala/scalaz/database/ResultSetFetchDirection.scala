// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{FETCH_FORWARD, FETCH_REVERSE, FETCH_UNKNOWN}

/**
 * ResultSet fetch direction.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ResultSetFetchDirection {
  def asInt: Int
}
/**
 * <code>ResultSet.FETCH_FORWARD</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object Forward extends ResultSetFetchDirection {
  def asInt = FETCH_FORWARD
}
/**
 * <code>ResultSet.FETCH_REVERSE</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object Reverse extends ResultSetFetchDirection {
  def asInt = FETCH_REVERSE
}
/**
 * <code>ResultSet.FETCH_UNKNOWN</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object Unknown extends ResultSetFetchDirection {
  def asInt = FETCH_UNKNOWN
}

/**
 * Functions over ResultSet fetch directions.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ResultSetFetchDirection {
  /**
   * All ResultSet fetch direction values.
   */
  def resultSetFetchDirections = List(Forward, Reverse, Unknown)

  /**
   * Returns a ResultSet fetch direction for the given value if one exists.
   */
  def fromInt(n: Int) = resultSetFetchDirections find (_.asInt == n)
}
