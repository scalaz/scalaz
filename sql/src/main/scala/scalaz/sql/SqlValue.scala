package scalaz.sql

import SqlValue._

sealed trait SqlValue[F[_], A] {
  val value: F[Either[SqlException, A]]
}

object SqlValue extends SqlValues

trait SqlValues {
  type SqlException =
  java.sql.SQLException
}