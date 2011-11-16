package scalaz
package std.effect

import effect.{IO, Resource}

import java.io.OutputStream

trait OutputStreamInstances {
  implicit val outputStreamResource: Resource[OutputStream] = new Resource[OutputStream] {
    def close(r: OutputStream) = IO(r.close)
  }
}

object outputStream extends OutputStreamInstances
