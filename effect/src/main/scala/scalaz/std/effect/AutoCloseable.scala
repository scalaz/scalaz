package scalaz
package std.effect

import effect.Resource

trait AutoCloseableInstances0 {
  implicit def autoCloseableResource[A <: java.lang.AutoCloseable]: Resource[A] =
    Resource.resourceFromAutoCloseable
}

trait AutoCloseableInstances extends AutoCloseableInstances0 {
}

object autoCloseable extends AutoCloseableInstances
