package scalaz
package sql

import SqlValue._

sealed trait SqlValue[F[_], A] {
  val value: EitherT[SqlException, F, A]
}

object SqlValue extends SqlValues

trait SqlValues {
  type SqlException =
  java.sql.SQLException
}
