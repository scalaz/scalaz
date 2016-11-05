package scalaz
package std.effect

import effect.Resource

import java.io.Closeable

trait CloseableInstances {
  implicit def closeableResource[A <: Closeable]: Resource[A] =
    Resource.resourceFromCloseable
}

object closeable extends CloseableInstances
