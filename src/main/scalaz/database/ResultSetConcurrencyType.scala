// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.ResultSet.{ CONCUR_READ_ONLY, CONCUR_UPDATABLE }

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
