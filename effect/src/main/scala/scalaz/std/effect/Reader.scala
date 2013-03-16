package scalaz
package std.effect

import effect.{IO, Resource}

import java.io.Reader

trait ReaderInstances {
  implicit val readerResource: Resource[Reader] = new Resource[Reader] {
    def close(r: Reader) = IO(r.close)
  }
}

object reader extends ReaderInstances
