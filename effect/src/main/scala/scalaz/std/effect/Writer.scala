package scalaz
package std.effect

import effect.Resource

import java.io.Writer

@deprecated("Use CloseableInstances", "7.1")
trait WriterInstances {
  implicit val writerResource: Resource[Writer] = 
    Resource.resourceFromCloseable
}

@deprecated("Use closeable", "7.1")
object writer extends WriterInstances
