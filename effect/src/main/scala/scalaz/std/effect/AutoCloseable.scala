package scalaz
package std.effect

import effect.Resource

trait AutoCloseableInstances {
  implicit def autoCloseableResource[A <: java.lang.AutoCloseable]: Resource[A] =
    Resource.resourceFromAutoCloseable
}

object autoCloseable extends AutoCloseableInstances
