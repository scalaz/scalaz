// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{ CONCUR_READ_ONLY, CONCUR_UPDATABLE }

/**
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
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ResultSetConcurrencyType {
  def resultSetConcurrencyTypes = List(ReadOnly, Updatable)

  def fromInt(n: Int) = resultSetConcurrencyTypes find (_.asInt == n)
}
