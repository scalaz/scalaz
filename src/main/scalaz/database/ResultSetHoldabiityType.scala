// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{ HOLD_CURSORS_OVER_COMMIT, CLOSE_CURSORS_AT_COMMIT }

/**
 * ResultSet holdability.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ResultSetHoldabilityType {
  def asInt: Int
}
/**
 * <code>ResultSet.HOLD_CURSORS_OVER_COMMIT</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object CursorsOverCommit extends ResultSetHoldabilityType {
  def asInt = HOLD_CURSORS_OVER_COMMIT
}
/**
 * <code>ResultSet.CLOSE_CURSORS_AT_COMMIT</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object CloseCursorsAtCommit extends ResultSetHoldabilityType {
  def asInt = CLOSE_CURSORS_AT_COMMIT
}

/**
 * Functions over ResultSet holdability.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ResultSetHoldabilityType {
  /**
   * All ResultSet holdability values.
   */
  def resultSetHoldabilityTypes = List(CursorsOverCommit, CloseCursorsAtCommit)

  /**
   * Returns a ResultSet holdability for the given value if one exists.
   */
  def fromInt(n: Int) = resultSetHoldabilityTypes find (_.asInt == n)
}
