package scalaz.database

sealed trait ResultSetHoldabilityType {
  def asInt: Int
}
final case object CursorsOverCommit extends ResultSetHoldabilityType {
  def asInt = HOLD_CURSORS_OVER_COMMIT
}
final case object CloseCursorsAtCommit extends ResultSetHoldabilityType {
  def asInt = CLOSE_CURSORS_AT_COMMIT
}

object ResultSetHoldabilityType {
  def resultSetHoldabilityTypes = List(CursorsOverCommit, CloseCursorsAtCommit)

  def fromInt(n: Int) = resultSetHoldabilityTypes find (_.asInt == n)
}
