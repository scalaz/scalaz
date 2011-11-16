package scalaz
package std.effect.sql

import effect.{IO, Resource}

import java.sql.Statement

trait StatementInstances {
  implicit val statementResource: Resource[Statement] = new Resource[Statement] {
    def close(r: Statement) = IO(r.close)
  }
}

object statement extends StatementInstances
