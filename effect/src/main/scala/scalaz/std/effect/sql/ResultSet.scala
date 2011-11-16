package scalaz
package std.effect.sql

import effect.{IO, Resource}

import java.sql.ResultSet

trait ResultSetInstances {
  implicit val resultSetResource: Resource[ResultSet] = new Resource[ResultSet] {
    def close(r: ResultSet) = IO(r.close)
  }
}

object resultSet extends ResultSetInstances
