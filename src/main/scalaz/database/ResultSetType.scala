// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{ TYPE_FORWARD_ONLY, TYPE_SCROLL_INSENSITIVE, TYPE_SCROLL_SENSITIVE }

/**
 * ResultSet type.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ResultSetType {
  def asInt: Int
}
/**
 * <code>ResultSet.TYPE_FORWARD_ONLY</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object ForwardOnly extends ResultSetType {
  def asInt = TYPE_FORWARD_ONLY
}
/**
 * <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object ScrollInsensitive extends ResultSetType {
  def asInt = TYPE_SCROLL_INSENSITIVE
}
/**
 * <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object ScrollSensitive extends ResultSetType {
  def asInt = TYPE_SCROLL_SENSITIVE
}

/**
 * Functions over ResultSet type.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ResultSetType {
  /**
   * All ResultSet type values.
   */
  def resultSetTypes = List(ForwardOnly, ScrollInsensitive, ScrollSensitive)

  /**
   * Returns a ResultSet type for the given value if one exists.
   */
  def fromInt(n: Int) = resultSetTypes find (_.asInt == n)
}
