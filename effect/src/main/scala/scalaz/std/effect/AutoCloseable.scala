package scalaz
package std.effect

import effect.Resource

trait AutoCloseableInstances0 {
  implicit def autoCloseableResource[A <: java.lang.AutoCloseable]: Resource[A] =
    Resource.resourceFromAutoCloseable
}

trait AutoCloseableInstances extends AutoCloseableInstances0 {
  /*
   * for scala-js compatibility
   * @see [[https://github.com/scala-js/scala-js/blob/v0.6.14/javalib/src/main/scala/java/io/Closeable.scala#L3]]
   */
  implicit def closeableResource[A <: java.io.Closeable]: Resource[A] =
    Resource.resourceFromCloseable
}

object autoCloseable extends AutoCloseableInstances
