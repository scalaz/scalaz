package scalaz
package std.effect

import effect.Resource

import java.io.OutputStream

@deprecated("Use CloseableInstances", "7.1")
trait OutputStreamInstances {
  implicit val outputStreamResource: Resource[OutputStream] = 
    Resource.resourceFromCloseable
}

@deprecated("Use closeable", "7.1")
object outputStream extends OutputStreamInstances
