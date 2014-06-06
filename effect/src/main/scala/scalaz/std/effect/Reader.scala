package scalaz
package std.effect

import effect.Resource

import java.io.Reader

@deprecated("Use CloseableInstances", "7.1")
trait ReaderInstances {
  implicit val readerResource: Resource[Reader] = 
    Resource.resourceFromCloseable
}

@deprecated("Use closeable", "7.1")
object reader extends ReaderInstances
