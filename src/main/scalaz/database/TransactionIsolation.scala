// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.Connection. { TRANSACTION_READ_UNCOMMITTED, TRANSACTION_READ_COMMITTED, TRANSACTION_REPEATABLE_READ, TRANSACTION_SERIALIZABLE }

/**
 * Transaction isolation.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait TransactionIsolation {
  def asInt: Int
}
/**
 * <code>Connection.TRANSACTION_READ_UNCOMMITTED</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object ReadUncommited extends TransactionIsolation {
  def asInt = TRANSACTION_READ_UNCOMMITTED
}
/**
 * <code>Connection.TRANSACTION_READ_COMMITTED</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object ReadCommited extends TransactionIsolation {
  def asInt = TRANSACTION_READ_COMMITTED
}
/**
 * <code>Connection.TRANSACTION_REPEATABLE_READ</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object RepeatableRead extends TransactionIsolation {
  def asInt = TRANSACTION_REPEATABLE_READ
}
/**
 * <code>Connection.TRANSACTION_SERIALIZABLE</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object TransactionSerializable extends TransactionIsolation {
  def asInt = TRANSACTION_SERIALIZABLE
}

/**
 * Functions over Transaction isolation.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object TransactionIsolation {
  /**
   * All Transaction isolation values.
   */
  def transactionIsolations = List(ReadUncommited, ReadCommited, RepeatableRead, TransactionSerializable)

  /**
   * Returns a Transaction isolation for the given value if one exists.
   */
  def fromInt(n: Int) = transactionIsolations find (_.asInt == n)
}
