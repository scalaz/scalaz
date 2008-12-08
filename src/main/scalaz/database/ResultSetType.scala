package scalaz.database

import java.sql.ResultSet.{ TYPE_FORWARD_ONLY, TYPE_SCROLL_INSENSITIVE, TYPE_SCROLL_SENSITIVE }

sealed trait ResultSetType {
  def asInt: Int
}
final case object ForwardOnly extends ResultSetType {
  def asInt = TYPE_FORWARD_ONLY
}
final case object ScrollInsensitive extends ResultSetType {
  def asInt = TYPE_SCROLL_INSENSITIVE
}
final case object ScrollSensitive extends ResultSetType {
  def asInt = TYPE_SCROLL_SENSITIVE
}

object ResultSetType {
  def resultSetTypes = List(ForwardOnly, ScrollInsensitive, ScrollSensitive)

  def fromInt(n: Int) = resultSetTypes find (_.asInt == n)
}
