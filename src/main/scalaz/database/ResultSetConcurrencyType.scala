// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{ CONCUR_READ_ONLY, CONCUR_UPDATABLE }

/**
 * ResultSet concurrency.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ResultSetConcurrencyType {
  def asInt: Int
}
/**
 * <code>ResultSet.CONCUR_READ_ONLY</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object ReadOnly extends ResultSetConcurrencyType {
  def asInt = CONCUR_READ_ONLY
}
/**
 * <code>ResultSet.CONCUR_UPDATABLE</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object Updatable extends ResultSetConcurrencyType {
  def asInt = CONCUR_UPDATABLE
}

/**
 * Functions over ResultSet concurrency.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ResultSetConcurrencyType {
  /**
   * All ResultSet concurrency values.
   */
  def resultSetConcurrencyTypes = List(ReadOnly, Updatable)

  /**
   * Returns a ResultSet concurrency for the given value if one exists.
   */
  def fromInt(n: Int) = resultSetConcurrencyTypes find (_.asInt == n)
}
