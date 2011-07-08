package scalaz
package sql

import SqlValueT._

sealed trait SqlValueT[F[_], A] {
  val value: EitherT[SqlException, F, A]
}

object SqlValueT extends SqlValueTs

trait SqlValueTs {
  type SqlException =
  java.sql.SQLException

  type SqlValue[A] =
  SqlValueT[Identity, A]
}
