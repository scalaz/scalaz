// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{FETCH_FORWARD, FETCH_REVERSE, FETCH_UNKNOWN}

sealed trait ResultSetFetchDirection {
  def asInt: Int
}
final case object Forward extends ResultSetFetchDirection {
  def asInt = FETCH_FORWARD
}
final case object Reverse extends ResultSetFetchDirection {
  def asInt = FETCH_REVERSE
}
final case object Unknown extends ResultSetFetchDirection {
  def asInt = FETCH_UNKNOWN
}

object ResultSetFetchDirection {
  def resultSetFetchDirections = List(Forward, Reverse, Unknown)

  def fromInt(n: Int) = resultSetFetchDirections find (_.asInt == n)
}
