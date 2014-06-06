package scalaz
package std.effect

import effect.Resource

import java.io.InputStream

@deprecated("Use CloseableInstances", "7.1")
trait InputStreamInstances {
  implicit val inputStreamResource: Resource[InputStream] = 
    Resource.resourceFromCloseable
}

@deprecated("Use closeable", "7.1")
object inputStream extends InputStreamInstances
