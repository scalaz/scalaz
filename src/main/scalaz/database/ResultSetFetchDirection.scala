// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{FETCH_FORWARD, FETCH_REVERSE, FETCH_UNKNOWN}

/**
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
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ResultSetFetchDirection {
  def resultSetFetchDirections = List(Forward, Reverse, Unknown)

  def fromInt(n: Int) = resultSetFetchDirections find (_.asInt == n)
}
