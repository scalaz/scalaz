// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.Connection. { TRANSACTION_READ_UNCOMMITTED, TRANSACTION_READ_COMMITTED, TRANSACTION_REPEATABLE_READ, TRANSACTION_SERIALIZABLE }

/**
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
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object TransactionIsolation {
  def transactionIsolations = List(ReadUncommited, ReadCommited, RepeatableRead, TransactionSerializable)

  def fromInt(n: Int) = transactionIsolations find (_.asInt == n)
}
