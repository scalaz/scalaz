package scalaz.database

import java.sql.Connection. { TRANSACTION_READ_UNCOMMITTED, TRANSACTION_READ_COMMITTED, TRANSACTION_REPEATABLE_READ, TRANSACTION_SERIALIZABLE }

sealed trait TransactionIsolation {
  def asInt: Int
}
final case object ReadUncommited extends TransactionIsolation {
  def asInt = TRANSACTION_READ_UNCOMMITTED
}
final case object ReadCommited extends TransactionIsolation {
  def asInt = TRANSACTION_READ_COMMITTED
}
final case object RepeatableRead extends TransactionIsolation {
  def asInt = TRANSACTION_REPEATABLE_READ
}
final case object TransactionSerializable extends TransactionIsolation {
  def asInt = TRANSACTION_SERIALIZABLE
}

object TransactionIsolation {
  def transactionIsolations = List(ReadUncommited, ReadCommited, RepeatableRead, TransactionSerializable)

  def fromInt(n: Int) = transactionIsolations find (_.asInt == n)
}
