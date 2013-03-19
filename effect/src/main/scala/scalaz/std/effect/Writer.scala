package scalaz
package std.effect

import effect.{IO, Resource}

import java.io.Writer

trait WriterInstances {
  implicit val writerResource: Resource[Writer] = new Resource[Writer] {
    def close(r: Writer) = IO(r.close)
  }
}

object writer extends WriterInstances
