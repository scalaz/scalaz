package scalaz.database

sealed trait ResultSetConcurrencyType {
  def asInt: Int
}
final case object ReadOnly extends ResultSetConcurrencyType {
  def asInt = CONCUR_READ_ONLY
}
final case object Updatable extends ResultSetConcurrencyType {
  def asInt = CONCUR_UPDATABLE
}

object ResultSetConcurrencyType {
  def resultSetConcurrencyTypes = List(ReadOnly, Updatable)

  def fromInt(n: Int) = resultSetConcurrencyTypes find (_.asInt == n)
}
