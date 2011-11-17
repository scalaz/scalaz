package scalaz
package std.effect.sql

import effect.{IO, Resource}

import java.sql.Connection

trait ConnectionInstances {
  implicit val connectionResource: Resource[Connection] = new Resource[Connection] {
    def close(r: Connection) = IO(r.close)
  }
}

object connection extends ConnectionInstances
