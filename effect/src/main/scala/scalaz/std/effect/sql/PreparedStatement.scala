package scalaz
package std.effect.sql

import effect.{IO, Resource}

import java.sql.PreparedStatement

trait PreparedStatementInstances {
  implicit val preparedStatementResource: Resource[PreparedStatement] = new Resource[PreparedStatement] {
    def close(r: PreparedStatement) = IO(r.close)
  }
}

object preparedStatement extends PreparedStatementInstances
